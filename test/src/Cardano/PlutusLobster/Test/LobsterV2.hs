{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Cardano.PlutusLobster.Test.LobsterV2 where

import           Control.Monad                         hiding (fmap)
import           Data.Default                          (Default (..))
import qualified Data.Map                              as Map
import           Data.Maybe                            (mapMaybe)
import           Data.Text                             (Text)
import           Ledger
import           Ledger.Ada                            as Ada
import           Ledger.Constraints                    as Constraints
import           Ledger.Scripts                        as Scripts
import           Ledger.Value                          as Value
import           Plutus.Contract                       as Contract
import           Plutus.Contract.Trace                 (InitialDistribution)
import           Plutus.Trace.Emulator                 as Emulator
import           Plutus.V1.Ledger.Api                  (toBuiltinData)
import           PlutusTx.Prelude                      hiding (Semigroup(..), mapMaybe, unless)
import           Prelude                               (IO, Semigroup(..), Show (..), String)
import           Text.Printf                           (printf)
import           Wallet.Emulator.Wallet

import           Cardano.PlutusLobster.LobsterPolicies
import           Cardano.PlutusLobster.LobsterV2Script

deployLobster :: LobsterParams -> Contract () EmptySchema Text ()
deployLobster lp = do
    let nft     = assetClassValue (lpNFT lp) 1
        c       = Constraints.mustPayToTheScript 0 nft
    ledgerTx <- submitTxConstraints (typedLobsterValidator lp) c
    awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String "deployed lobster"

vote :: LobsterParams -> Integer -> Contract () EmptySchema Text ()
vote lp v = do
    let counter = assetClassValue (lpCounter lp) v
        c       = Constraints.mustPayToTheScript 0 (counter <> lovelaceValueOf (lpFee lp)) <>
                  Constraints.mustMintValue counter
        lookups = Constraints.typedValidatorLookups (typedRequestValidator lp) <>
                  Constraints.mintingPolicy otherPolicy
    ledgerTx <- submitTxConstraintsWith lookups c
    awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo $ "voted " ++ show v

findLobster :: LobsterParams -> Contract () EmptySchema Text (Maybe (TxOutRef, ChainIndexTxOut, Integer, Integer))
findLobster lp = do
    utxos <- utxosAt $ scriptAddress $ lobsterValidator lp
    let m = go $ Map.toList utxos
    Contract.logInfo @String $ case m of
        Nothing                   -> "lobster not found"
        Just (_, _, votes, count) -> printf "found lobster, %d votes, count %d" votes count
    return m
  where
    go :: [(TxOutRef, ChainIndexTxOut)] -> Maybe (TxOutRef, ChainIndexTxOut, Integer, Integer)
    go [] = Nothing
    go ((oref, o) : xs) = case f o of
        Nothing -> go xs
        Just y  -> Just (oref, o, fst y, snd y)

    f :: ChainIndexTxOut -> Maybe (Integer, Integer)
    f (ScriptChainIndexTxOut _ _ (Right (Datum d)) v)
        | d == zeroBD && assetClassValueOf v (lpNFT lp) == 1 =
            Just ( assetClassValueOf v $ lpVotes lp
                 , assetClassValueOf v $ lpCounter lp
                 )
    f _ = Nothing

process :: LobsterParams -> Contract () EmptySchema Text ()
process lp = do
    let addr = scriptAddress $ requestValidator lp
    m <- findLobster lp
    case m of
        Nothing                      -> Contract.logInfo @String "lobster not found"
        Just (oref, o, votes, count) -> do
            reqs <- mapMaybe f . Map.toList <$> utxosAt addr
            let reqs'  = take (lpVoteCount lp - votes) reqs
                votes' = votes + length reqs'
            Contract.logInfo @String $ printf "found %d requests, using %d" (length reqs) (length reqs')
            case reqs' of
                [] -> return ()
                _  -> do
                    let count'  = count + sum ((\(_, _, c) -> c) <$> reqs')
                    let c       = Constraints.mustPayToTheScript 0 (
                                    assetClassValue (lpNFT     lp) 1      <>
                                    assetClassValue (lpCounter lp) count' <>
                                    assetClassValue (lpVotes   lp) votes')                                    <>
                                  Constraints.mustSpendScriptOutput oref (Redeemer zeroBD)                    <>
                                  Constraints.mustMintValue (assetClassValue (lpVotes   lp) (votes' - votes)) <>
                                  mconcat
                                    [ mustSpendScriptOutput oref' (Redeemer zeroBD)
                                    | (oref', _, _) <- reqs'
                                    ]
                        lookups = typedValidatorLookups (typedLobsterValidator lp)                                  <>
                                  otherScript (requestValidator lp)                                                 <>
                                  otherScript (lobsterValidator lp)                                                 <>
                                  mintingPolicy otherPolicy                                                         <>
                                  Constraints.unspentOutputs (Map.fromList [(oref', o') | (oref', o', _) <- reqs']) <>
                                  Constraints.unspentOutputs (Map.singleton oref o)
                    ledgerTx <- submitTxConstraintsWith lookups c
                    awaitTxConfirmed $ txId ledgerTx
                    Contract.logInfo @String $ printf "processed %d votes" votes'

                    return ()
  where
    f :: (TxOutRef, ChainIndexTxOut) -> Maybe (TxOutRef, ChainIndexTxOut, Integer)
    f (oref, o) = do
        n <- g o
        return (oref, o, n)

    g :: ChainIndexTxOut -> Maybe Integer
    g (ScriptChainIndexTxOut _ _ (Right (Datum d)) v) =
        let c = assetClassValueOf v $ lpCounter lp
        in  if d == zeroBD                                &&
               assetClassValueOf v lovelaceAC >= lpFee lp &&
               c >= 1                                     &&
               c <= 100
                then Just c
                else Nothing
    g _ = Nothing

test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig
    { _initialChainState = Left initDist
    , _slotConfig        = def
    , _feeConfig         = def
    }
  where
    initDist :: InitialDistribution
    initDist = Map.fromList
        [ (knownWallet 1, Ada.lovelaceValueOf 100_000_000 <> assetClassValue nftAC 1)
        , (knownWallet 2, Ada.lovelaceValueOf 100_000_000)
        , (knownWallet 3, Ada.lovelaceValueOf 100_000_000)
        , (knownWallet 4, Ada.lovelaceValueOf 100_000_000)
        ]

myTrace :: EmulatorTrace ()
myTrace = do
    let lp = LobsterParams
                { lpSeed      = 42
                , lpNameCount = 100
                , lpVoteCount = 10
                , lpFee       = 1_000_000
                , lpNFT       = nftAC
                , lpCounter   = AssetClass (otherSymbol, counterTokenName)
                , lpVotes     = AssetClass (otherSymbol, votesTokenName)
                }
    void $ activateContractWallet (knownWallet 1) $ deployLobster lp
    void $ Emulator.waitNSlots 1

    forM_ [2 .. 4] $ \w ->
        activateContractWallet (knownWallet w) $ vote lp 17
    void $ Emulator.waitNSlots 1

    void $ activateContractWallet (knownWallet 1) $ process lp
    void $ Emulator.waitNSlots 1

    void $ activateContractWallet (knownWallet 1) $ void $ findLobster lp
    void $ Emulator.waitNSlots 1

nftAC, lovelaceAC :: AssetClass
nftAC      = AssetClass ("ff", nftTokenName)
lovelaceAC = AssetClass (adaSymbol, adaToken)

otherSymbol :: CurrencySymbol
otherSymbol = mpsSymbol $ Scripts.mintingPolicyHash otherPolicy

zeroBD :: BuiltinData
zeroBD = toBuiltinData (0 :: Integer)
