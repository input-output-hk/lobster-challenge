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

module Cardano.PlutusLobster.LobsterV2Script
  ( apiLobsterScript
  , apiRequestScript
  , LobsterParams (..)
  , typedLobsterValidator
  , lobsterValidator
  , typedRequestValidator
  , requestValidator
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
import           Plutus.V1.Ledger.Bytes   (bytes)
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

mkRequestValidator :: LobsterParams -> Integer -> Integer -> ScriptContext -> Bool
mkRequestValidator lp _ _ ctx = any (\i -> assetClassValueOf (txOutValue $ txInInfoResolved i) (lpNFT lp) == 1) $ txInfoInputs $ scriptContextTxInfo ctx
    -- Check whether the NFT is present in an input of the transaction beeing validated. That NFT "sits" in the state carrying UTxO of the main lobster contract,
    -- We can therefore be sure that the main lobster validator will be executed.

data Requesting
instance Scripts.ValidatorTypes Requesting where
    type instance DatumType Requesting = Integer
    type instance RedeemerType Requesting = Integer

typedRequestValidator :: LobsterParams -> Scripts.TypedValidator Requesting
typedRequestValidator lp = Scripts.mkTypedValidator @Requesting
    ($$(PlutusTx.compile [|| mkRequestValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode lp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @Integer

requestValidator :: LobsterParams -> Validator
requestValidator = Scripts.validatorScript . typedRequestValidator

requestScript :: LobsterParams -> Plutus.Script
requestScript = Ledger.unValidatorScript . requestValidator

requestScriptAsShortBs :: LobsterParams -> SBS.ShortByteString
requestScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . requestScript

apiRequestScript :: LobsterParams -> PlutusScript PlutusScriptV1
apiRequestScript = PlutusScriptSerialised . requestScriptAsShortBs

expectedDatumHash :: DatumHash
expectedDatumHash = DatumHash $ toBuiltin $ bytes "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314" -- hash of 0

mkLobsterValidator :: DatumHash -> LobsterParams -> Integer -> BuiltinData -> ScriptContext -> Bool
mkLobsterValidator h lp _ _ ctx
    | oldNFT == 1 =                                          -- Are we validating the "special" UTxO carrying the state?
        (txOutDatumHash ownOutput == Just h)              && -- The datum of the "updated" UTxO should be 0.
        (newNFT   == 1)                                   && -- The "updated" UTxO must contain the NFT.

        if | oldVotes < voteCount  ->                        -- Is voting still in progress?
                (newCounter == oldCounter + snd requests) && -- Is the new counter correct?
                (newVotes   == oldVotes   + fst requests) && -- Is the new number of votes correct?
                (newVotes   <= voteCount)                    -- Is the new number of votes <= the maximal number of votes?

           | oldVotes == voteCount ->                        -- Is voting finished, but our "secret" number has not yet been added?
                (newCounter == finalCounter)              && -- Has the final result been calculated correctly?
                (newVotes   == 1 + voteCount)                -- Have the new votes been calculated correctly?

           | otherwise             ->                        -- Is voting finished, and our "secret" number has been added?
                (newCounter == oldCounter)                && -- Has the final counter value been kept?
                (newVotes   == oldVotes)                     -- Has the number of votes been kept?

    | otherwise   = True                                     -- If we don't have the UTxO with the NFT, we don't care.
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
    !inVal  = txOutValue ownInput
    !outVal = txOutValue ownOutput

    oldNFT, newNFT, oldCounter, newCounter, oldVotes, newVotes, finalCounter, voteCount :: Integer
    oldNFT       = assetClassValueOf inVal  nftAC
    newNFT       = assetClassValueOf outVal nftAC
    oldCounter   = assetClassValueOf inVal  counterAC
    newCounter   = assetClassValueOf outVal counterAC
    !oldVotes    = assetClassValueOf inVal  votesAC
    !newVotes    = assetClassValueOf outVal votesAC
    finalCounter = (lpSeed lp + oldCounter) `modInteger` lpNameCount lp
    voteCount    = lpVoteCount lp
    fees         = lpFee lp

    nftAC, counterAC, votesAC, lovelace :: AssetClass
    nftAC     = lpNFT     lp
    counterAC = lpCounter lp
    votesAC   = lpVotes   lp
    lovelace  = AssetClass (adaSymbol, adaToken)

    requests :: (Integer, Integer)                       -- Calculates the number of valid votes and their sum.
    requests = foldr f (0, 0) $ txInfoInputs $ scriptContextTxInfo ctx
      where
        f :: TxInInfo -> (Integer, Integer) -> (Integer, Integer)
        f i (!votes, !counter) =
          let
            !v = txOutValue $ txInInfoResolved i
            !c = assetClassValueOf v counterAC
          in
            if (assetClassValueOf v lovelace >= fees) && -- Is the fee included?
               (txInInfoOutRef i /= ownRef)           && -- This must be a vote, not the state carrying UTxO we are currently validating.
               (c >= 1)                               && -- The vote must be at least 1.
               (c <= 100)                                -- The vote must be at most 100.
                then (votes + 1, counter + c)
                else (votes, counter)

    ownRef :: TxOutRef
    ownRef = case scriptContextPurpose ctx of
        Spending ref -> ref
        _            -> error ()

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
