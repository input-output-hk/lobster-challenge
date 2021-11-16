{-# LANGUAGE BangPatterns #-}
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
  ( apiLobsterScript
  , apiRequestScript
  , LobsterParams (..)
  , mkLobsterValidator
  , typedLobsterValidator
  , lobsterValidator
  , typedRequestValidator
  , requestValidator
  , requestValidatorHash
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
import           Plutus.V1.Ledger.Bytes   (bytes)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V1.Ledger.Credential (Credential (..))
import           Prelude                  (Show)

data LobsterParams = LobsterParams
    { lpSeed           :: !Integer
    , lpNFT            :: !AssetClass
    , lpCounter        :: !AssetClass
    , lpFinished       :: !AssetClass
    , lpNameCount      :: !Integer
    , lpDeadline       :: !POSIXTime
    , lpBatcher        :: !PubKeyHash
    } deriving Show

PlutusTx.makeLift ''LobsterParams

{- HLINT ignore "Avoid lambda" -}

{-# INLINEABLE scriptInput #-}
scriptInput :: ScriptContext -> TxOut
scriptInput (ScriptContext t_info (Spending o_ref)) = getScriptInput (txInfoInputs t_info) o_ref
scriptInput _ = traceError "script input not found !!!"

{-# INLINEABLE getScriptInput #-}
getScriptInput :: [TxInInfo] -> TxOutRef -> TxOut
getScriptInput [] _ = traceError "script input not found !!!"
getScriptInput ((TxInInfo tref ot) : tl) o_ref
  | tref == o_ref = ot
  | otherwise = getScriptInput tl o_ref


{-# INLINEABLE scriptOutputs #-}
scriptOutputs :: ScriptContext -> [TxOut]
scriptOutputs ctx = getScriptOutputs ctx (txInfoOutputs $ scriptContextTxInfo ctx) []


{-# INLINEABLE getScriptOutputs #-}
getScriptOutputs :: ScriptContext -> [TxOut] -> [TxOut] -> [TxOut]
getScriptOutputs _ [] acc = acc
getScriptOutputs ctx (o@(TxOut (Address (ScriptCredential s) _) _ _) : tl) acc
  | (isScriptCredential ctx s) = getScriptOutputs ctx tl (o : acc)
  | otherwise = getScriptOutputs ctx tl acc
getScriptOutputs ctx (_ : tl) acc = getScriptOutputs ctx tl acc

{-# INLINEABLE isScriptCredential #-}
isScriptCredential :: ScriptContext -> ValidatorHash -> Bool
isScriptCredential ctx o_s =
  case scriptInput ctx of
    (TxOut (Address (ScriptCredential i_s) _) _ _) -> i_s == o_s
    _ -> False


{-# INLINEABLE isSignedBy #-}
isSignedBy :: TxInfo -> PubKeyHash -> Bool
isSignedBy txInfo pk = go pk (txInfoSignatories txInfo)

  where
    go :: PubKeyHash -> [PubKeyHash] -> Bool
    go _ [] = False
    go k (p : tl)
     | k == p = True
     | otherwise = go k tl

{-# INLINABLE mkRequestValidator #-}
mkRequestValidator :: AssetClass -> PubKeyHash -> Integer -> ScriptContext -> Bool
mkRequestValidator _ _ _ _ = True


data Requesting
instance Scripts.ValidatorTypes Requesting where
    type instance DatumType Requesting = PubKeyHash
    type instance RedeemerType Requesting = Integer

typedRequestValidator :: AssetClass -> Scripts.TypedValidator Requesting
typedRequestValidator rp = Scripts.mkTypedValidator @Requesting
    ($$(PlutusTx.compile [|| mkRequestValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode rp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @Integer

requestValidator :: AssetClass -> Validator
requestValidator = Scripts.validatorScript . typedRequestValidator

requestValidatorHash :: AssetClass -> ValidatorHash
requestValidatorHash = Scripts.validatorHash . typedRequestValidator

requestScript :: AssetClass -> Plutus.Script
requestScript = Ledger.unValidatorScript . requestValidator

requestScriptAsShortBs :: AssetClass -> SBS.ShortByteString
requestScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . requestScript

apiRequestScript :: AssetClass -> PlutusScript PlutusScriptV1
apiRequestScript = PlutusScriptSerialised . requestScriptAsShortBs


expectedDatumHash :: DatumHash
expectedDatumHash = DatumHash $ toBuiltin $ bytes "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314" -- hash of 0


{-# INLINABLE mkLobsterValidator #-}
mkLobsterValidator :: DatumHash -> LobsterParams -> Integer -> Integer -> ScriptContext -> Bool
mkLobsterValidator h (LobsterParams seed nftAC cntAC endAC n_count deadline pkBatcher) d _ ctx@(ScriptContext ctxInfo _) =
  traceIfFalse "Wrong Batcher" (isSignedBy ctxInfo pkBatcher)                             &&
  if getInValue nftAC == 1 then
    traceIfFalse "Input datum must be zero"  (d == 0)                                     &&
    traceIfFalse "Output datum must be zero" (txOutDatumHash (ownOutput ctx) == Just h)   &&
    traceIfFalse "NFT missing from output"   (getOutValue nftAC == 1)                     &&
    traceIfFalse "already finished"          (oldFinished == 0)                           &&
     if | to deadline `contains` valRange ->
           traceIfFalse "not yet finished"   (newFinished == 0)                           &&
           traceIfFalse "counter mismatched" ( nbCounters == newCounter)


        | from deadline `contains` valRange ->
            traceIfFalse "finished token missing from output" (newFinished == 1)           &&
            traceIfFalse "wrong final counter value"          (newCounter == finalCounter)

        | otherwise                         ->
            traceError "Invalid validity interval"

   else
    True -- If it is the "wrong" UTxO (not having the NFT), we don't care.

  where

    getCounters :: AssetClass -> Integer -> [TxInInfo] -> Integer
    getCounters _ acc [] = acc
    getCounters counterAC acc ((TxInInfo _ ot) : tl) =
      let !c = assetClassValueOf (txOutValue ot) counterAC in
        getCounters counterAC (acc + c) tl

    inVal :: Value
    !inVal = txOutValue (scriptInput ctx)

    getInValue :: AssetClass -> Integer
    getInValue ac = assetClassValueOf inVal ac

    ownOutput :: ScriptContext -> TxOut
    ownOutput sctx =
      case scriptOutputs sctx of
        [o] -> o
        _ -> traceError "invalid lobster output !!!"

    outVal :: Value
    !outVal = txOutValue $ ownOutput ctx


    getOutValue :: AssetClass -> Integer
    getOutValue ac = assetClassValueOf outVal ac

    oldFinished, newFinished, oldCounter, newCounter, finalCounter :: Integer
    !oldFinished = getInValue endAC
    !newFinished = getOutValue endAC
    !oldCounter = getInValue cntAC
    !newCounter = getOutValue cntAC
    !finalCounter = (seed + oldCounter) `modInteger` n_count

    valRange :: POSIXTimeRange
    !valRange = (txInfoValidRange ctxInfo)

    nbCounters :: Integer
    nbCounters = getCounters cntAC 0 (txInfoInputs ctxInfo)


data LobsterNaming
instance Scripts.ValidatorTypes LobsterNaming where
    type instance DatumType LobsterNaming = Integer
    type instance RedeemerType LobsterNaming = Integer

typedLobsterValidator :: LobsterParams -> Scripts.TypedValidator LobsterNaming
typedLobsterValidator lp = Scripts.mkTypedValidator @LobsterNaming
    ($$(PlutusTx.compile [|| mkLobsterValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode expectedDatumHash
        `PlutusTx.applyCode` PlutusTx.liftCode lp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @Integer

lobsterValidator :: LobsterParams -> Validator
lobsterValidator = Scripts.validatorScript . typedLobsterValidator

lobsterScript :: LobsterParams -> Plutus.Script
lobsterScript = Ledger.unValidatorScript . lobsterValidator

lobsterScriptAsShortBs :: LobsterParams -> SBS.ShortByteString
lobsterScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . lobsterScript

apiLobsterScript :: LobsterParams -> PlutusScript PlutusScriptV1
apiLobsterScript = PlutusScriptSerialised . lobsterScriptAsShortBs
