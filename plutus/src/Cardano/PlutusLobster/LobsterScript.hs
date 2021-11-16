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
  ( pkhFromDatum
  , apiLobsterScript
  , apiRequestScript
  , apiEndScript
  , LobsterParams (..)
  , RequestParams (..)
  , mkLobsterValidator
  , typedLobsterValidator
  , lobsterValidator
  , typedRequestValidator
  , requestValidator
  , requestValidatorHash
  , typedEndValidator
  , endValidator
  , endValidatorHash
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
    , lpTicketSymbol   :: !CurrencySymbol
    , lpNameCount      :: !Integer
    , lpDeadline       :: !POSIXTime
    , lpBatcher        :: !PubKeyHash
    } deriving Show

PlutusTx.makeLift ''LobsterParams

{- HLINT ignore "Avoid lambda" -}

{-# INLINEABLE pkhFromDatum #-}
pkhFromDatum :: DatumHash -> [(DatumHash, Datum)] -> Maybe PubKeyHash
pkhFromDatum _ [] = Nothing
pkhFromDatum dh ((dh', Datum d) : tl)
  | dh == dh' = PlutusTx.fromBuiltinData d
  | otherwise = pkhFromDatum dh tl


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



data RequestParams = RequestParams
    { rpNFT            :: !AssetClass -- should be same NFT AssetClass as LobsterParams : to guarantee uniqueness only
    , rTicketSymbol    :: !CurrencySymbol -- currency symbol for ticket token
    , rpBatcher        :: !PubKeyHash -- should be same pubKeyHash as LobsterParams
    , rEndHash         :: !ValidatorHash --- hash of end script to which order receipt should be sent
    } deriving Show

PlutusTx.makeLift ''RequestParams

{-# INLINABLE mkRequestValidator #-}
-- several uxtos sitting at request script can be consumed in one transaction
-- but there cannot be two utxos for the same pubkeyhash datum. Otherwise,
-- batcher can claim fee for more than one vote performed by same voter by
-- providing only one minted receipt.
mkRequestValidator :: RequestParams -> PubKeyHash -> Integer -> ScriptContext -> Bool
mkRequestValidator (RequestParams _ tkSymbol pkBatcher eHash) pkVoter _ ctx@(ScriptContext ctxInfo _) =
 traceIfFalse "Wrong batcher" (isSignedBy ctxInfo pkBatcher) &&
 traceIfFalse "Only one utxo per pubkeyhash expected in input" (onlyOneUtxoPerPKH ctx ctxInfo pkVoter (txInfoInputs ctxInfo) 0) &&
 traceIfFalse "One ticket token per pubkeyhash expected in input" (oneInputToken ctxInfo tkSymbol) &&
 traceIfFalse "Ticket Token sent to burning script" (ticketTokenToEndScript ctxInfo eHash tkSymbol)

 where
   oneInputToken :: TxInfo -> CurrencySymbol -> Bool
   oneInputToken t_info cSymbol =
     ( valueOf (valueSpent t_info) cSymbol ticketToken ) == 1

   ticketToken :: TokenName
   ticketToken = TokenName (getPubKeyHash pkVoter)


   ticketTokenToEndScript :: TxInfo -> ValidatorHash -> CurrencySymbol -> Bool
   ticketTokenToEndScript t_info vh cSymbol =
     ( valueOf (valueLockedBy t_info vh) cSymbol ticketToken ) == 1

   onlyOneUtxoPerPKH :: ScriptContext -> TxInfo -> PubKeyHash -> [TxInInfo] -> Integer -> Bool
   onlyOneUtxoPerPKH _ _ _ [] nb = nb == 1
   onlyOneUtxoPerPKH sc t_info pkh ((TxInInfo _ (TxOut (Address (ScriptCredential s) _) _ (Just dh))) : tl) nb
     | (isScriptCredential sc s) =
       case pkhFromDatum dh (txInfoData t_info) of
         Nothing -> traceError "Invalid public key hash datum !!!"
         Just k -> if k == pkh then
                     onlyOneUtxoPerPKH sc t_info pkh tl (nb + 1)
                   else
                     onlyOneUtxoPerPKH sc t_info pkh tl nb
     | otherwise = traceError "Only request script expected as input !!!"
   onlyOneUtxoPerPKH sc t_info pkh (_ : tl) nb = onlyOneUtxoPerPKH sc t_info pkh tl nb


data Requesting
instance Scripts.ValidatorTypes Requesting where
    type instance DatumType Requesting = PubKeyHash
    type instance RedeemerType Requesting = Integer

typedRequestValidator :: RequestParams -> Scripts.TypedValidator Requesting
typedRequestValidator rp = Scripts.mkTypedValidator @Requesting
    ($$(PlutusTx.compile [|| mkRequestValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode rp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @Integer

requestValidator :: RequestParams -> Validator
requestValidator = Scripts.validatorScript . typedRequestValidator

requestValidatorHash :: RequestParams -> ValidatorHash
requestValidatorHash = Scripts.validatorHash . typedRequestValidator

requestScript :: RequestParams -> Plutus.Script
requestScript = Ledger.unValidatorScript . requestValidator

requestScriptAsShortBs :: RequestParams -> SBS.ShortByteString
requestScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . requestScript

apiRequestScript :: RequestParams -> PlutusScript PlutusScriptV1
apiRequestScript = PlutusScriptSerialised . requestScriptAsShortBs



-- parameterized with NFT AssetClass used for LobsterScript to guarantee uniqueness
{-# INLINABLE mkEndValidator #-}
mkEndValidator :: AssetClass -> Integer -> Integer -> ScriptContext -> Bool
mkEndValidator _ _ _ _ = False -- burning address

data Ending
instance Scripts.ValidatorTypes Ending where
    type instance DatumType Ending = Integer
    type instance RedeemerType Ending = Integer

typedEndValidator :: AssetClass -> Scripts.TypedValidator Ending
typedEndValidator ac = Scripts.mkTypedValidator @Ending
    ($$(PlutusTx.compile [|| mkEndValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode ac)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @Integer

endValidator :: AssetClass -> Validator
endValidator = Scripts.validatorScript . typedEndValidator

endValidatorHash :: AssetClass -> ValidatorHash
endValidatorHash = Scripts.validatorHash . typedEndValidator

endScript :: AssetClass -> Plutus.Script
endScript = Ledger.unValidatorScript . endValidator

endScriptAsShortBs :: AssetClass -> SBS.ShortByteString
endScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . endScript

apiEndScript :: AssetClass -> PlutusScript PlutusScriptV1
apiEndScript = PlutusScriptSerialised . endScriptAsShortBs


expectedDatumHash :: DatumHash
expectedDatumHash = DatumHash $ toBuiltin $ bytes "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314" -- hash of 0


{-# INLINABLE mkLobsterValidator #-}
mkLobsterValidator :: DatumHash -> LobsterParams -> Integer -> Integer -> ScriptContext -> Bool
mkLobsterValidator h (LobsterParams seed nftAC cntAC endAC tkSymbol n_count deadline pkBatcher) d _ ctx@(ScriptContext ctxInfo _) =
  traceIfFalse "Wrong batcher" (isSignedBy ctxInfo pkBatcher)                             &&
  if getInValue nftAC == 1 then
    traceIfFalse "Input datum must be zero"  (d == 0)                                     &&
    traceIfFalse "Output datum must be zero" (txOutDatumHash (ownOutput ctx) == Just h)   &&
    traceIfFalse "NFT missing from output"   (getOutValue nftAC == 1)                     &&
    traceIfFalse "already finished"          (oldFinished == 0)                           &&
     if | to deadline `contains` valRange ->
           traceIfFalse "not yet finished"   (newFinished == 0)                           &&
           traceIfFalse "counter mismatched" ( nbCounters == newCounter )


        | from deadline `contains` valRange ->
            traceIfFalse "finished token missing from output" (newFinished == 1)           &&
            traceIfFalse "wrong final counter value"          (newCounter == finalCounter)

        | otherwise                         ->
            traceError "Invalid validity interval"

   else
    True -- If it is the "wrong" UTxO (not having the NFT), we don't care.

  where

    -- checks counter and ticket tokens
    getCounters :: AssetClass -> Integer -> [TxInInfo] -> Integer
    getCounters _ acc [] = acc
    getCounters counterAC acc ((TxInInfo _ (TxOut (Address (PubKeyCredential k) _) v _)) : tl) =
      let !c = assetClassValueOf v counterAC in
      let !t = valueOf v tkSymbol (TokenName (getPubKeyHash k)) in
        if t == 1 then
          getCounters counterAC (acc + c) tl
        else
          traceError "Ticket Token not present !!!"

    getCounters counterAC acc (_ : tl) = getCounters counterAC acc tl


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
    nbCounters = (getCounters cntAC 0 (txInfoInputs ctxInfo)) + oldCounter


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
