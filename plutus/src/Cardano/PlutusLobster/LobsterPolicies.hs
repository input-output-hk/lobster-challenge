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

module Cardano.PlutusLobster.LobsterPolicies
  ( apiNFTMintScript
  , apiOtherMintScript
  , apiTicketMintScript
  , PolicyParams (..)
  , PolicyMintingAction (..)
  , nftTokenName
  , counterTokenName
  , finishedTokenName
  , otherPolicy
  , otherPolicyHash
  , ticketPolicy
  , ticketPolicyHash
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
import           Cardano.PlutusLobster.LobsterScript as LobsterScript

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

nftTokenName, counterTokenName, finishedTokenName :: TokenName
nftTokenName = "LobsterNFT"
counterTokenName = "LobsterCounter"
finishedTokenName = "LobsterFinished"

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
  = MintCounters -- to mint counter tokens when submitting votes
  | MintFinish  -- to mint finish token when vote deadline reached
  | MintReceipt -- to mint receipt token when vote processed successfully
  deriving (Show, Generic, FromJSON, ToJSON)

instance Eq PolicyMintingAction where
  {-# INLINABLE (==) #-}
  MintCounters == MintCounters = True
  MintFinish == MintFinish = True
  MintReceipt == MintReceipt = True
  _ == _ = False


data PolicyParams = PolicyParams
   {  pCounterTokenName   :: !TokenName
    , pFinishedTokenName  :: !TokenName
    , pSubmitFees         :: !Integer -- min fee required by batcher to submit request
    , pCollectFees        :: !Integer -- min fee collected by batcher after successful process
    , pRequestHash        :: !ValidatorHash
    , pNFT                :: !AssetClass
    , pDeadline           :: !POSIXTime
   } deriving Show
-- The sum of pSumbitFees + pCollectFees corresponds to the fees claimed by batcher.

PlutusTx.makeLift ''PolicyParams


{-# INLINABLE lovelace #-}
lovelace :: AssetClass
lovelace = AssetClass (adaSymbol, adaToken)

{-# INLINABLE mkOtherPolicy #-}
mkOtherPolicy :: PolicyParams -> BuiltinData -> ScriptContext -> Bool
mkOtherPolicy (PolicyParams tkCount tkFinish sFees cFees rHash nftAC deadline) redeemer
              (ScriptContext (TxInfo txIn txOuts _ txMint _ _ valRange txSig txData _) (Minting cs)) =
  case action redeemer of
   MintCounters ->
     let counterAC = AssetClass (cs, tkCount)
         c = assetClassValueOf txMint counterAC
         dh = requestScriptDatum rHash cFees txOuts -- trigger error if request script not found in output with collect fee
     in
       c >= 1 && -- valid counter conditions
       c <= 100 &&
       to deadline `contains` valRange && -- deadline not expired
       traceIfFalse "Token/Fee not sent to submitter !!!" (tokenAndFeeToSubmitter counterAC txSig sFees c txOuts) &&
       ( case LobsterScript.pkhFromDatum dh txData of
           Nothing -> traceIfFalse "Invalid Public Key Hash !!!" False
           Just pk -> [pk] == txSig
       ) -- pubkeyhash sent to request script

   MintFinish ->
     onlyOneMintedFinishToken cs tkFinish txMint && -- only one finish token minted
     existsLobsterNFT nftAC txIn -- lobster nft present as input and only executed by batcher (i.e., enforced by validator script)

   MintReceipt ->
     existsLobsterNFT nftAC txIn -- lobster nft present as input and only executed by batcher (i.e., enforced by validator script)

  where
    action :: BuiltinData -> PolicyMintingAction
    action r =
      case PlutusTx.fromBuiltinData r of
        Nothing -> traceError "Invalid policy redeemer !!!"
        Just a -> a

    tokenAndFeeToSubmitter :: AssetClass -> [PubKeyHash] -> Integer -> Integer -> [TxOut] -> Bool
    tokenAndFeeToSubmitter _ _ _ _ [] = False
    tokenAndFeeToSubmitter counterAC sig_l p_fees nbC ((TxOut (Address (PubKeyCredential k) _) v _) : tl)
      | sig_l == [k] && (assetClassValueOf v counterAC) == nbC && (assetClassValueOf v lovelace) >= p_fees = True
      | otherwise = tokenAndFeeToSubmitter counterAC sig_l p_fees nbC tl
    tokenAndFeeToSubmitter counterAC sig_l p_fees nbC (_ : tl) = tokenAndFeeToSubmitter counterAC sig_l p_fees nbC tl


    requestScriptDatum :: ValidatorHash -> Integer -> [TxOut] -> DatumHash
    requestScriptDatum _ _ [] = traceError "Request Script Output Expected !!"
    requestScriptDatum rh c_fees ((TxOut (Address (ScriptCredential s) _) v (Just dh)) : _)
      | s == rh && (assetClassValueOf v lovelace) >= c_fees = dh -- only one script output expected
      | otherwise = traceError "Only one script output expected !!!"
    requestScriptDatum rh c_fees (_ : tl) = requestScriptDatum rh c_fees tl


    onlyOneMintedFinishToken :: CurrencySymbol -> TokenName -> Value -> Bool
    onlyOneMintedFinishToken cur tn (Value mp) =
      case Map.lookup cur mp of
        Nothing -> False
        Just mp' -> case Map.toList mp' of
                      [(tn', i)] -> tn' == tn && i == 1
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


-- need to change for production code
PlutusTx.unstableMakeIsData ''PolicyMintingAction


-- policy used to generate ticket request with tokenName set to pubKeyHash of miner
-- parameterized with NFT AssetClass used for LobsterScript
{-# INLINABLE mkTicketPolicy #-}
mkTicketPolicy :: AssetClass -> BuiltinData -> ScriptContext -> Bool
mkTicketPolicy _ _ (ScriptContext (TxInfo _ _ _ txMint _ _ _ txSig _ _) _) =
   traceIfFalse "Invalid ticket" (checkMintedTicket txMint txSig)

   where
     checkMintedTicket :: Value -> [PubKeyHash] -> Bool
     checkMintedTicket v [pk] =
       case flattenValue v of
         [(_, tn, amt)] -> (unTokenName tn == getPubKeyHash pk) && amt == 1
         _              -> False
     checkMintedTicket _ _ = traceError "Only one signatory expected !!!"


ticketPolicy :: AssetClass -> TScripts.MintingPolicy
ticketPolicy ac = mkMintingPolicyScript $
              $$(PlutusTx.compile [|| \a' -> TScripts.wrapMintingPolicy $ mkTicketPolicy a' ||])
              `PlutusTx.applyCode` PlutusTx.liftCode ac


ticketPolicyHash :: AssetClass ->  Scripts.MintingPolicyHash
ticketPolicyHash = Scripts.mintingPolicyHash . ticketPolicy

ticketPlutusScript :: AssetClass -> Script
ticketPlutusScript = unMintingPolicyScript . ticketPolicy

ticketValidator :: AssetClass -> Validator
ticketValidator = Validator . ticketPlutusScript

ticketScriptAsShortBs :: AssetClass -> SBS.ShortByteString
ticketScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . ticketValidator

apiTicketMintScript :: AssetClass -> PlutusScript PlutusScriptV1
apiTicketMintScript = PlutusScriptSerialised . ticketScriptAsShortBs
