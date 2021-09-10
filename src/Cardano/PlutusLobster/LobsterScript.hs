{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.PlutusLobster.LobsterScript
  ( apiNFTMintScript
  , apiOtherMintScript
  , apiLobsterScript
  , LobsterParams (..)
  , nftTokenName
  , counterTokenName
  , finishedTokenName
  ) where

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import qualified PlutusTx
import           PlutusTx.Builtins        (modInteger)
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Prelude                  (Show)

data LobsterParams = LobsterParams
    { lpSeed      :: Integer
    , lpNFT       :: AssetClass
    , lpCounter   :: AssetClass
    , lpFinished  :: AssetClass
    , lpDeadline  :: POSIXTime
    , lpNameCount :: Integer
    } deriving Show

PlutusTx.makeLift ''LobsterParams

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

nftPolicy :: TxOutRef -> Scripts.MintingPolicy
nftPolicy utxo = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \tn utxo' -> Scripts.wrapMintingPolicy $ mkNFTPolicy tn utxo' ||])
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

{-# INLINABLE mkOtherPolicy #-}
mkOtherPolicy :: BuiltinData -> ScriptContext -> Bool
mkOtherPolicy _ _ = True

otherPolicy :: Scripts.MintingPolicy
otherPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkOtherPolicy ||])

otherPlutusScript :: Script
otherPlutusScript = unMintingPolicyScript otherPolicy

otherValidator :: Validator
otherValidator = Validator otherPlutusScript

otherScriptAsCbor :: LB.ByteString
otherScriptAsCbor = serialise otherValidator

apiOtherMintScript :: PlutusScript PlutusScriptV1
apiOtherMintScript = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict otherScriptAsCbor

mkLobsterValidator :: LobsterParams -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkLobsterValidator lp _ _ ctx =
    traceIfFalse "NFT missing from input"  (assetClassValueOf inVal  (lpNFT lp)      == 1) &&
    traceIfFalse "NFT missing from output" (assetClassValueOf outVal (lpNFT lp)      == 1) &&
    traceIfFalse "already finished"        (assetClassValueOf inVal  (lpFinished lp) == 0) &&
    if | to (lpDeadline lp) `contains` txValidRange   ->
            traceIfFalse "must not finish"            (assetClassValueOf outVal  (lpFinished lp) == 0)                       &&
            traceIfFalse "counter increase too small" (increase >= 1)                                                        &&
            traceIfFalse "counter increase too large" (increase <= 100)
       | from (lpDeadline lp) `contains` txValidRange ->
            traceIfFalse "finshed marker missing"     (assetClassValueOf outVal (lpFinished lp) == 1)                        &&
            traceIfFalse "wrong counter value"        (newCounter == ((lpSeed lp + oldCounter) `modInteger` lpNameCount lp))
       | otherwise                                    -> traceError "invalid validity range"
  where
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "lobster input missing"
        Just i  -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one lobster output"

    inVal, outVal :: Value
    inVal = txOutValue ownInput
    outVal = txOutValue ownOutput

    oldCounter, newCounter, increase :: Integer
    oldCounter = assetClassValueOf inVal (lpCounter lp)
    newCounter = assetClassValueOf outVal (lpCounter lp)
    increase = newCounter - oldCounter

    txValidRange :: POSIXTimeRange
    txValidRange = txInfoValidRange $ scriptContextTxInfo ctx

data LobsterNaming
instance Scripts.ValidatorTypes LobsterNaming where
    type instance DatumType LobsterNaming = BuiltinData
    type instance RedeemerType LobsterNaming = BuiltinData

typedLobsterValidator :: LobsterParams -> Scripts.TypedValidator LobsterNaming
typedLobsterValidator lp = Scripts.mkTypedValidator @LobsterNaming
    ($$(PlutusTx.compile [|| mkLobsterValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode lp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @BuiltinData @BuiltinData

lobsterValidator :: LobsterParams -> Validator
lobsterValidator = Scripts.validatorScript . typedLobsterValidator

lobsterScript :: LobsterParams -> Plutus.Script
lobsterScript = Ledger.unValidatorScript . lobsterValidator

lobsterScriptAsShortBs :: LobsterParams -> SBS.ShortByteString
lobsterScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . lobsterScript

apiLobsterScript :: LobsterParams -> PlutusScript PlutusScriptV1
apiLobsterScript = PlutusScriptSerialised . lobsterScriptAsShortBs
