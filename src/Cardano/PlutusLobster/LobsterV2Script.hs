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

module Cardano.PlutusLobster.LobsterV2Script
  ( apiLobsterScript
  , apiRequestScript
  , LobsterParams (..)
  ) where

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import           Ledger.Ada
import qualified PlutusTx
import           PlutusTx.Builtins        (modInteger)
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Prelude                  (Show)

data LobsterParams = LobsterParams
    { lpSeed      :: !Integer
    , lpNFT       :: !AssetClass
    , lpCounter   :: !AssetClass
    , lpVotes     :: !AssetClass
    , lpNameCount :: !Integer
    , lpVoteCount :: !Integer
    , lpFee       :: !Integer
    } deriving Show

PlutusTx.makeLift ''LobsterParams

{- HLINT ignore "Avoid lambda" -}

mkRequestValidator :: LobsterParams -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkRequestValidator lp _ _ ctx =
    traceIfFalse "lobster input missing" $
        any (\i -> assetClassValueOf (txOutValue $ txInInfoResolved i) (lpNFT lp) == 1) $ txInfoInputs $ scriptContextTxInfo ctx

data Requesting
instance Scripts.ValidatorTypes Requesting where
    type instance DatumType Requesting = BuiltinData
    type instance RedeemerType Requesting = BuiltinData

typedRequestValidator :: LobsterParams -> Scripts.TypedValidator Requesting
typedRequestValidator lp = Scripts.mkTypedValidator @Requesting
    ($$(PlutusTx.compile [|| mkRequestValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode lp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @BuiltinData @BuiltinData

requestValidator :: LobsterParams -> Validator
requestValidator = Scripts.validatorScript . typedRequestValidator

requestScript :: LobsterParams -> Plutus.Script
requestScript = Ledger.unValidatorScript . requestValidator

requestScriptAsShortBs :: LobsterParams -> SBS.ShortByteString
requestScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . requestScript

apiRequestScript :: LobsterParams -> PlutusScript PlutusScriptV1
apiRequestScript = PlutusScriptSerialised . requestScriptAsShortBs

expectedDatumHash :: DatumHash
expectedDatumHash = DatumHash "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314" -- hash of 0

mkLobsterValidator :: Address -> DatumHash -> LobsterParams -> Integer -> BuiltinData -> ScriptContext -> Bool
mkLobsterValidator addr h lp _ _ ctx
    | oldNFT == 1 =
        traceIfFalse "output datum must be zero" (txOutDatumHash ownOutput == Just h)       &&
        traceIfFalse "NFT missing from output"   (newNFT   == 1)                            &&
        if | oldVotes < lpVoteCount lp  ->
                traceIfFalse "wrong new counter" (newCounter == oldCounter + sum requests)  &&
                traceIfFalse "wrong new votes"   (newVotes   == oldVotes + length requests) &&
                traceIfFalse "too many votes"    (newVotes   <= lpVoteCount lp)
           | oldVotes == lpVoteCount lp ->
                traceIfFalse "wrong new counter" (newCounter == finalCounter)               &&
                traceIfFalse "wrong new votes"   (newVotes   == 1 + lpVoteCount lp)
           | otherwise                  ->
                traceIfFalse "wrong new counter" (newCounter == oldCounter)                 &&
                traceIfFalse "wrong new votes"   (newVotes   == oldVotes)
    | otherwise   = True -- If we don't have the UTxO with the NFT, we don't care.
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

    oldNFT, newNFT, oldCounter, newCounter, oldVotes, newVotes, finalCounter :: Integer
    oldNFT       = assetClassValueOf inVal  $ lpNFT lp
    newNFT       = assetClassValueOf outVal $ lpNFT lp
    oldCounter   = assetClassValueOf inVal  $ lpCounter lp
    newCounter   = assetClassValueOf outVal $ lpCounter lp
    oldVotes     = assetClassValueOf inVal  $ lpVotes lp
    newVotes     = assetClassValueOf outVal $ lpVotes lp
    finalCounter = (lpSeed lp + oldCounter) `modInteger` lpNameCount lp

    lovelace :: AssetClass
    lovelace = AssetClass (adaSymbol, adaToken)

    requests :: [Integer]
    requests = [ c
               | i <- txInfoInputs $ scriptContextTxInfo ctx
               , let o = txInInfoResolved i
               , txOutAddress o == addr
               , let v = txOutValue o
               , let l = assetClassValueOf v lovelace
               , l >= lpFee lp
               , let c = assetClassValueOf v $ lpCounter lp
               , c >= 1 && c <= 100 -- vote in valid range 1 - 100
               ]

data LobsterNaming
instance Scripts.ValidatorTypes LobsterNaming where
    type instance DatumType LobsterNaming = Integer
    type instance RedeemerType LobsterNaming = BuiltinData

typedLobsterValidator :: LobsterParams -> Scripts.TypedValidator LobsterNaming
typedLobsterValidator lp = Scripts.mkTypedValidator @LobsterNaming
    ($$(PlutusTx.compile [|| mkLobsterValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode (scriptAddress $ requestValidator lp)
        `PlutusTx.applyCode` PlutusTx.liftCode expectedDatumHash
        `PlutusTx.applyCode` PlutusTx.liftCode lp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @BuiltinData

lobsterValidator :: LobsterParams -> Validator
lobsterValidator = Scripts.validatorScript . typedLobsterValidator

lobsterScript :: LobsterParams -> Plutus.Script
lobsterScript = Ledger.unValidatorScript . lobsterValidator

lobsterScriptAsShortBs :: LobsterParams -> SBS.ShortByteString
lobsterScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . lobsterScript

apiLobsterScript :: LobsterParams -> PlutusScript PlutusScriptV1
apiLobsterScript = PlutusScriptSerialised . lobsterScriptAsShortBs
