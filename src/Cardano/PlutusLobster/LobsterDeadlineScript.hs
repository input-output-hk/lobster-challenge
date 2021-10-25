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

module Cardano.PlutusLobster.LobsterDeadlineScript
  ( apiLobsterDeadlineScript
  , LobsterDeadlineParams (..)
  , finishedTokenName
  , expectedDatumHash
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
import           Prelude                  (Show)

finishedTokenName :: TokenName
finishedTokenName = "LobsterFinished"

expectedDatumHash :: DatumHash
expectedDatumHash = -- datumHash $ Datum $ PlutusTx.toBuiltinData (0 :: Integer)
    DatumHash $ toBuiltin $ bytes "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314"

data LobsterDeadlineParams = LobsterDeadlineParams
    { ldpSeed      :: !Integer
    , ldpNFT       :: !AssetClass
    , ldpCounter   :: !AssetClass
    , ldpFinished  :: !AssetClass
    , ldpNameCount :: !Integer
    , ldpDeadline  :: !POSIXTime
    } deriving Show

PlutusTx.makeLift ''LobsterDeadlineParams

{- HLINT ignore "Avoid lambda" -}

mkLobsterValidator :: DatumHash -> LobsterDeadlineParams -> Integer -> Integer -> ScriptContext -> Bool
mkLobsterValidator h lp d _ ctx =
    if oldNFT == 1 then
        traceIfFalse "input datum must be zero"  (d == 0)                             &&
        traceIfFalse "output datum must be zero" (txOutDatumHash ownOutput == Just h) &&
        traceIfFalse "NFT missing from output"   (newNFT      == 1)                   &&
        traceIfFalse "already finished"          (oldFinished == 0)                   &&
        if | to (ldpDeadline lp) `contains` valInt   ->
                traceIfFalse "not yet finished"             (newFinished == 0)       &&
                traceIfFalse "counter increase too small"   (increase >= 1)          &&
                traceIfFalse "counter increase too large"   (increase <= 100)
           | from (ldpDeadline lp) `contains` valInt ->
                traceIfFalse "finished missing from output" (newFinished == 1)       &&
                traceIfFalse "wrong final counter value"    (newCounter == expected)
           | otherwise                               ->
                traceError "invalid validity interval"
    else True -- If it is the "wrong" UTxO (not having the NFT), we don't care.
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

    oldNFT, newNFT, oldCounter, newCounter, increase,expected :: Integer
    oldNFT      = assetClassValueOf inVal  $ ldpNFT lp
    newNFT      = assetClassValueOf outVal $ ldpNFT lp
    oldCounter  = assetClassValueOf inVal  $ ldpCounter lp
    newCounter  = assetClassValueOf outVal $ ldpCounter lp
    oldFinished = assetClassValueOf inVal  $ ldpFinished lp
    newFinished = assetClassValueOf outVal $ ldpFinished lp
    increase    = newCounter - oldCounter
    expected    = (ldpSeed lp + oldCounter) `modInteger` ldpNameCount lp

    valInt :: POSIXTimeRange
    valInt = txInfoValidRange $ scriptContextTxInfo ctx

data LobsterNaming
instance Scripts.ValidatorTypes LobsterNaming where
    type instance DatumType LobsterNaming = Integer
    type instance RedeemerType LobsterNaming = Integer

typedLobsterValidator :: LobsterDeadlineParams -> Scripts.TypedValidator LobsterNaming
typedLobsterValidator lp = Scripts.mkTypedValidator @LobsterNaming
    ($$(PlutusTx.compile [|| mkLobsterValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode expectedDatumHash
        `PlutusTx.applyCode` PlutusTx.liftCode lp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @Integer

lobsterValidator :: LobsterDeadlineParams -> Validator
lobsterValidator = Scripts.validatorScript . typedLobsterValidator

lobsterScript :: LobsterDeadlineParams -> Plutus.Script
lobsterScript = Ledger.unValidatorScript . lobsterValidator

lobsterScriptAsShortBs :: LobsterDeadlineParams -> SBS.ShortByteString
lobsterScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . lobsterScript

apiLobsterDeadlineScript :: LobsterDeadlineParams -> PlutusScript PlutusScriptV1
apiLobsterDeadlineScript = PlutusScriptSerialised . lobsterScriptAsShortBs
