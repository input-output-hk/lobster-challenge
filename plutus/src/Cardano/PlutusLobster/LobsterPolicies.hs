{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.PlutusLobster.LobsterPoliciesV4
  ( apiNFTMintScript
  , apiOtherMintScript
  , PolicyParams (..)
  , PolicyMintingAction (..)
  , nftTokenName
  , counterTokenName
  , votesTokenName
  , otherPolicy
  , otherPolicyHash
  ) where

import           Cardano.Api.Shelley   (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Short as SBS
import           Data.Aeson            (FromJSON, ToJSON)
import           GHC.Generics          (Generic)
import           Ledger                hiding (singleton)
import           Ledger.Scripts        as Scripts
import qualified Ledger.Typed.Scripts  as TScripts
import           Ledger.Value          as Value
import           Ledger.Ada
import           PlutusTx.AssocMap     as Map
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (Semigroup (..), unless)
import           Plutus.V1.Ledger.Credential (Credential (..))
import           Prelude               (Show)
{- HLINT ignore "Avoid lambda" -}

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: TokenName -> TxOutRef -> BuiltinData -> ScriptContext -> Bool
mkNFTPolicy tn utxo _ ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                            traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

nftTokenName, counterTokenName, votesTokenName :: TokenName
nftTokenName = "LobsterNFT"
counterTokenName = "LobsterCounter"
votesTokenName = "LobsterVotes"

nftPolicy :: TxOutRef -> TScripts.MintingPolicy
nftPolicy utxo = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \tn utxo' -> TScripts.wrapMintingPolicy $ mkNFTPolicy tn utxo' ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode nftTokenName
    `PlutusTx.applyCode`
     PlutusTx.liftCode utxo

nftPlutusScript :: TxOutRef -> Script
nftPlutusScript = unMintingPolicyScript . nftPolicy

nftValidator :: TxOutRef -> Validator
nftValidator = Validator . nftPlutusScript

nftScriptAsCbor :: TxOutRef -> LB.ByteString
nftScriptAsCbor = serialise . nftValidator

apiNFTMintScript :: TxOutRef -> PlutusScript PlutusScriptV1
apiNFTMintScript = PlutusScriptSerialised . SBS.toShort . LB.toStrict . nftScriptAsCbor


-- | Action used when minting counters and votes tokens
data PolicyMintingAction
  = MintCounters
  | MintVotes
  deriving (Show, Generic, FromJSON, ToJSON)

instance Eq PolicyMintingAction where
  {-# INLINABLE (==) #-}
  MintCounters == MintCounters = True
  MintVotes == MintVotes = True
  _ == _ = False


data PolicyParams = PolicyParams
   { pCounterTokenName  :: !TokenName
    , pVotesTokenName   :: !TokenName
    , pFees             :: !Integer
    , pRequestHash      :: !ValidatorHash
    , pNFT              :: !AssetClass
   } deriving Show

PlutusTx.makeLift ''PolicyParams

{-# INLINABLE mkOtherPolicy #-}
mkOtherPolicy :: PolicyParams -> BuiltinData -> ScriptContext -> Bool
mkOtherPolicy (PolicyParams tkC tkV fees rHash nftAC) redeemer (ScriptContext (TxInfo txIn txOuts _ txMint _ _ _ txSig txData _) (Minting cs)) =
  case action redeemer of
   MintCounters ->
     let cAC = AssetClass (cs, tkC)
         c = assetClassValueOf txMint cAC
         dh = requestScriptDatum rHash txOuts
     in
       c >= 1 && -- valid counter conditions
       c <= 100 &&
       traceIfFalse "Token/Fee not sent to submitter !!!" (tokenAndFeeToSubmitter cAC txSig fees c txOuts) &&
       ( case getPubKeyHash dh txData of
           Nothing -> traceIfFalse "Invalid Public Key Hash !!!" False
           Just pk -> [pk] == txSig
       )

   MintVotes ->
     onlyMintedVoteTokens cs tkV txMint && -- only minting votes token
     existsLobsterNFT nftAC txIn -- lobster nft present as input

  where
    action :: BuiltinData -> PolicyMintingAction
    action r =
      case PlutusTx.fromBuiltinData r of
        Nothing -> traceError "Invalid policy redeemer !!!"
        Just a -> a

    tokenAndFeeToSubmitter :: AssetClass -> [PubKeyHash] -> Integer -> Integer -> [TxOut] -> Bool
    tokenAndFeeToSubmitter _ _ _ _ [] = False
    tokenAndFeeToSubmitter counterAC sig_l p_fees nbC ((TxOut (Address (PubKeyCredential k) _) v _) : tl)
      | sig_l == [k] && (assetClassValueOf v counterAC) == nbC && (assetClassValueOf v (AssetClass (adaSymbol, adaToken))) >= p_fees = True
      | otherwise = tokenAndFeeToSubmitter counterAC sig_l p_fees nbC tl
    tokenAndFeeToSubmitter counterAC sig_l p_fees nbC (_ : tl) = tokenAndFeeToSubmitter counterAC sig_l p_fees nbC tl

    getPubKeyHash :: DatumHash -> [(DatumHash, Datum)] -> Maybe PubKeyHash
    getPubKeyHash _ [] = Nothing
    getPubKeyHash dh ((dh', Datum d) : tl)
      | dh == dh' = PlutusTx.fromBuiltinData d
      | otherwise = getPubKeyHash dh tl

    requestScriptDatum :: ValidatorHash -> [TxOut] -> DatumHash
    requestScriptDatum _ [] = traceError "Request Script Output Expected !!"
    requestScriptDatum rh ((TxOut (Address (ScriptCredential s) _) _ (Just dh)) : _)
      | s == rh = dh -- only one script output expected
      | otherwise = traceError "Only one script output expected !!!"
    requestScriptDatum rh (_ : tl) = requestScriptDatum rh tl


    onlyMintedVoteTokens :: CurrencySymbol -> TokenName -> Value -> Bool
    onlyMintedVoteTokens cur tn (Value mp) =
      case Map.lookup cur mp of
        Nothing -> False
        Just mp' -> case Map.toList mp' of
                      [(tn', i)] -> tn' == tn && i > 0
                      _ -> False

    existsLobsterNFT :: AssetClass -> [TxInInfo] -> Bool
    existsLobsterNFT _ [] = False
    existsLobsterNFT nAC ((TxInInfo _ ot) : tl)
      | assetClassValueOf (txOutValue $ ot) nAC == 1 = True
      | otherwise = existsLobsterNFT nAC tl

mkOtherPolicy _ _ _  = False

otherPolicy :: PolicyParams -> TScripts.MintingPolicy
otherPolicy lp = mkMintingPolicyScript $
              $$(PlutusTx.compile [|| \lp' -> TScripts.wrapMintingPolicy $ mkOtherPolicy lp' ||])
              `PlutusTx.applyCode` PlutusTx.liftCode lp


otherPolicyHash :: PolicyParams -> Scripts.MintingPolicyHash
otherPolicyHash = Scripts.mintingPolicyHash . otherPolicy

otherPlutusScript :: PolicyParams -> Script
otherPlutusScript = unMintingPolicyScript . otherPolicy

otherValidator :: PolicyParams -> Validator
otherValidator = Validator . otherPlutusScript

otherScriptAsShortBs :: PolicyParams -> SBS.ShortByteString
otherScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . otherValidator

apiOtherMintScript :: PolicyParams -> PlutusScript PlutusScriptV1
apiOtherMintScript = PlutusScriptSerialised . otherScriptAsShortBs

PlutusTx.unstableMakeIsData ''PolicyMintingAction
