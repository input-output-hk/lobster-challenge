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

module Cardano.PlutusLobster.LobsterV2Script
  ( apiLobsterScript
  , LobsterParams (..)
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
    , lpVotes     :: AssetClass
    , lpNameCount :: Integer
    , lpVoteCount :: Integer
    } deriving Show

PlutusTx.makeLift ''LobsterParams

{- HLINT ignore "Avoid lambda" -}

expectedDatumHash :: DatumHash
expectedDatumHash = DatumHash "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314" -- hash of 0

mkLobsterValidator :: DatumHash -> LobsterParams -> Integer -> BuiltinData -> ScriptContext -> Bool
mkLobsterValidator h lp d _ ctx
    | oldNFT == 1 =
        traceIfFalse "input datum must be zero"  (d == 0)                             &&
        traceIfFalse "output datum must be zero" (txOutDatumHash ownOutput == Just h) &&
        traceIfFalse "NFT missing from output"   (newNFT   == 1)                      &&
        traceIfFalse "already finished"          (oldVotes <= lpVoteCount lp)         &&
        traceIfFalse "wrong new votes"           (newVotes == oldVotes + 1)           &&
        if oldVotes < lpVoteCount lp then
            traceIfFalse "counter increase too small" (increase >= 1)                                                        &&
            traceIfFalse "counter increase too large" (increase <= 100)
        else
            traceIfFalse "wrong counter value"        (newCounter == ((lpSeed lp + oldCounter) `modInteger` lpNameCount lp))
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

    oldNFT, newNFT, oldCounter, newCounter, increase, oldVotes, newVotes :: Integer
    oldNFT     = assetClassValueOf inVal  $ lpNFT lp
    newNFT     = assetClassValueOf outVal $ lpNFT lp
    oldCounter = assetClassValueOf inVal  $ lpCounter lp
    newCounter = assetClassValueOf outVal $ lpCounter lp
    oldVotes   = assetClassValueOf inVal  $ lpVotes lp
    newVotes   = assetClassValueOf outVal $ lpVotes lp
    increase   = newCounter - oldCounter

data LobsterNaming
instance Scripts.ValidatorTypes LobsterNaming where
    type instance DatumType LobsterNaming = Integer
    type instance RedeemerType LobsterNaming = BuiltinData

typedLobsterValidator :: LobsterParams -> Scripts.TypedValidator LobsterNaming
typedLobsterValidator lp = Scripts.mkTypedValidator @LobsterNaming
    ($$(PlutusTx.compile [|| mkLobsterValidator ||])
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
