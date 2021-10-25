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

module Cardano.PlutusLobster.LobsterVoteCountScript
  ( apiLobsterVoteCountScript
  , LobsterVoteCountParams (..)
  , votesTokenName
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

votesTokenName :: TokenName
votesTokenName = "LobsterVotes"

data LobsterVoteCountParams = LobsterVoteCountParams
    { lvcpSeed      :: Integer
    , lvcpNFT       :: AssetClass
    , lvcpCounter   :: AssetClass
    , lvcpVotes     :: AssetClass
    , lvcpNameCount :: Integer
    , lvcpVoteCount :: Integer
    } deriving Show

PlutusTx.makeLift ''LobsterVoteCountParams

{- HLINT ignore "Avoid lambda" -}

mkLobsterValidator :: LobsterVoteCountParams -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkLobsterValidator lp _ _ ctx =
    traceIfFalse "NFT missing from input"  (oldNFT   == 1)                &&
    traceIfFalse "NFT missing from output" (newNFT   == 1)                &&
    traceIfFalse "already finished"        (oldVotes <= lvcpVoteCount lp) &&
    traceIfFalse "wrong new votes"         (newVotes == oldVotes + 1)   &&
    if oldVotes < lvcpVoteCount lp then
        traceIfFalse "counter increase too small" (increase >= 1)                                                            &&
        traceIfFalse "counter increase too large" (increase <= 100)
    else
        traceIfFalse "wrong counter value"        (newCounter == ((lvcpSeed lp + oldCounter) `modInteger` lvcpNameCount lp))
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
    oldNFT     = assetClassValueOf inVal  $ lvcpNFT lp
    newNFT     = assetClassValueOf outVal $ lvcpNFT lp
    oldCounter = assetClassValueOf inVal  $ lvcpCounter lp
    newCounter = assetClassValueOf outVal $ lvcpCounter lp
    oldVotes   = assetClassValueOf inVal  $ lvcpVotes lp
    newVotes   = assetClassValueOf outVal $ lvcpVotes lp
    increase   = newCounter - oldCounter

data LobsterNaming
instance Scripts.ValidatorTypes LobsterNaming where
    type instance DatumType LobsterNaming = BuiltinData
    type instance RedeemerType LobsterNaming = BuiltinData

typedLobsterValidator :: LobsterVoteCountParams -> Scripts.TypedValidator LobsterNaming
typedLobsterValidator lp = Scripts.mkTypedValidator @LobsterNaming
    ($$(PlutusTx.compile [|| mkLobsterValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode lp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @BuiltinData @BuiltinData

lobsterValidator :: LobsterVoteCountParams -> Validator
lobsterValidator = Scripts.validatorScript . typedLobsterValidator

lobsterScript :: LobsterVoteCountParams -> Plutus.Script
lobsterScript = Ledger.unValidatorScript . lobsterValidator

lobsterScriptAsShortBs :: LobsterVoteCountParams -> SBS.ShortByteString
lobsterScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . lobsterScript

apiLobsterVoteCountScript :: LobsterVoteCountParams -> PlutusScript PlutusScriptV1
apiLobsterVoteCountScript = PlutusScriptSerialised . lobsterScriptAsShortBs
