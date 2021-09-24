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

import           Cardano.Api.Shelley         (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy        as LB
import qualified Data.ByteString.Short       as SBS
import           Ledger                      hiding (singleton)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import           Ledger.Ada
import qualified PlutusTx
import           PlutusTx.Builtins           (modInteger)
import           PlutusTx.Prelude            hiding (Semigroup (..), unless)
import           Plutus.V1.Ledger.Bytes      (bytes)
import qualified Plutus.V1.Ledger.Scripts    as Plutus
import           Plutus.V1.Ledger.Credential (Credential (..))
import           Prelude                     (Show)

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

data ResultAcc = ResultAcc !Integer !Integer

instance Eq ResultAcc where
  {-# INLINABLE (==) #-}
  ResultAcc v c == ResultAcc v' c' = (v == v') && (c == c')

mkLobsterValidator :: DatumHash -> LobsterParams -> Integer -> BuiltinData -> ScriptContext -> Bool
mkLobsterValidator h lp _ _ ctx
  | oldNFT == 1 =                              -- Are we validating the "special" UTxO carrying the state?
      (txOutDatumHash ownOutput == Just h)  && -- The datum of the "updated" UTxO should be 0.
      (newNFT == 1)                         && -- The "updated" UTxO must contain the NFT.
      validCounters

  | otherwise   = True                         -- If we don't have the UTxO with the NFT, we don't care.

  where
    ctxInfo :: TxInfo
    ctxInfo = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput =
      let in_l = txInfoInputs $ ctxInfo in
       case getScriptInput in_l of
        Nothing -> traceError "lobster input missing"
        Just o  -> o

    getScriptInput :: [TxInInfo] -> Maybe TxOut
    getScriptInput [] = Nothing
    getScriptInput (i : tl)
      | txInInfoOutRef i == ownRef = Just (txInInfoResolved i)
      | otherwise                  = getScriptInput tl

    ownOutput :: TxOut
    ownOutput =
      let out_l = txInfoOutputs $ ctxInfo in
        case getScriptOutputs out_l [] of
        [o] -> o
        _   -> traceError "expected exactly one lobster output"

    getScriptOutputs :: [TxOut] -> [TxOut] -> [TxOut]
    getScriptOutputs [] acc = acc
    getScriptOutputs (o@(TxOut (Address (ScriptCredential s) _) _ _) : tl) acc
      | s == scrHash              = getScriptOutputs tl (o : acc)
      | otherwise                 = getScriptOutputs tl acc
    getScriptOutputs (_ : tl) acc = getScriptOutputs tl acc

    -- use scrHash instead of ownHash
    scrHash :: ValidatorHash
    scrHash =
      case ownInput of
       TxOut (Address (ScriptCredential s) _) _ _ -> s
       _                                          -> error ()

    inVal, outVal :: Value
    !inVal  = txOutValue ownInput
    !outVal = txOutValue ownOutput

    oldNFT, newNFT, oldCounter, newCounter, oldVotes, newVotes, finalCounter, voteCount, fees :: Integer
    !oldNFT       = assetClassValueOf inVal  nftAC
    !newNFT       = assetClassValueOf outVal nftAC
    !oldCounter   = assetClassValueOf inVal  counterAC
    !newCounter   = assetClassValueOf outVal counterAC
    !oldVotes     = assetClassValueOf inVal  votesAC
    !newVotes     = assetClassValueOf outVal votesAC
    !finalCounter = (lpSeed lp + oldCounter) `modInteger` lpNameCount lp
    !voteCount    = lpVoteCount lp
    !fees         = lpFee lp

    nftAC, counterAC, votesAC, lovelace :: AssetClass
    !nftAC     = lpNFT     lp
    !counterAC = lpCounter lp
    !votesAC   = lpVotes   lp
    !lovelace  = AssetClass (adaSymbol, adaToken)

    requests :: ResultAcc
    requests =
      let in_l = txInfoInputs $ ctxInfo in
        requests' (ResultAcc oldVotes oldCounter) in_l

    requests' :: ResultAcc -> [TxInInfo] -> ResultAcc
    requests' acc [] = acc
    requests' acc@(ResultAcc vote counter) ((TxInInfo tref ot) : tl) =
      let !v = txOutValue ot
          !c = assetClassValueOf v counterAC
      in
        if (assetClassValueOf v lovelace >= fees) &&
           (tref /= ownRef)                       &&
           (c >= 1)                               &&
           (c <= 100)
        then requests' (ResultAcc (vote + 1) (counter + c)) tl
        else requests' acc tl

    validCounters :: Bool
    validCounters =
      if | oldVotes < voteCount  ->                       -- Is voting still in progress?
           (newVotes <= voteCount)                     && -- Is the new number of votes <= the maximal number of votes?
           (requests == ResultAcc newVotes newCounter)

         | oldVotes == voteCount ->                       -- Is voting finished, but our "secret" number has not yet been added?
             (newCounter == finalCounter)              && -- Has the final result been calculated correctly?
             (newVotes   == 1 + voteCount)                -- Have the new votes been calculated correctly?

         | otherwise             ->                       -- Is voting finished, and our "secret" number has been added?
             (newCounter == oldCounter)                && -- Has the final counter value been kept?
             (newVotes   == oldVotes)                     -- Has the number of votes been kept?


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
