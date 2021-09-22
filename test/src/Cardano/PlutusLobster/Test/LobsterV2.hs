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
import           Data.Text                             (Text)
import           Ledger
import           Ledger.Ada                            as Ada
import           Ledger.Constraints                    as Constraints
import           Ledger.Scripts                        as Scripts
import           Ledger.Value                          as Value
import           Plutus.Contract                       as Contract
import           Plutus.Contract.Trace                 (InitialDistribution)
import           Plutus.Trace.Emulator                 as Emulator
import           PlutusTx.Prelude                      hiding (Semigroup(..), unless)
import           Prelude                               (IO, Semigroup(..), Show (..), String)
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

test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig
        { _initialChainState = Left initDist
        , _slotConfig        = def
        , _feeConfig         = def
        }

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
    _ <- activateContractWallet (knownWallet 1) $ deployLobster lp
    void $ Emulator.waitNSlots 1

    forM_ [2 .. 4] $ \w ->
        activateContractWallet (knownWallet w) $ vote lp 17
    void $ Emulator.waitNSlots 1

nftAC :: AssetClass
nftAC = AssetClass ("ff", nftTokenName)

otherSymbol :: CurrencySymbol
otherSymbol = mpsSymbol $ Scripts.mintingPolicyHash otherPolicy

{-
import           Week06.Oracle.Core
import           Week06.Oracle.Funds
import           Week06.Oracle.Swap

assetSymbol :: CurrencySymbol
assetSymbol = "ff"

assetToken :: TokenName
assetToken = "USDT"

test :: IO ()
test = runEmulatorTraceIO' def emCfg def myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 10]]

    v :: Value
    v = Ada.lovelaceValueOf                    100_000_000 <>
        Value.singleton assetSymbol assetToken 100_000_000

checkOracle :: Oracle -> Contract () Empty Text a
checkOracle oracle = do
    m <- findOracle oracle
    case m of
        Nothing        -> return ()
        Just (_, _, x) -> Contract.logInfo $ "Oracle value: " ++ show x
    Contract.waitNSlots 1 >> checkOracle oracle

myTrace :: EmulatorTrace ()
myTrace = do
    let op = OracleParams
                { opFees = 1_000_000
                , opSymbol = assetSymbol
                , opToken  = assetToken
                }

    h1 <- activateContractWallet (Wallet 1) $ runOracle op
    void $ Emulator.waitNSlots 1
    oracle <- getOracle h1

    void $ activateContractWallet (Wallet 2) $ checkOracle oracle

    callEndpoint @"update" h1 1_500_000
    void $ Emulator.waitNSlots 3

    void $ activateContractWallet (Wallet 1) ownFunds'
    void $ activateContractWallet (Wallet 3) ownFunds'
    void $ activateContractWallet (Wallet 4) ownFunds'
    void $ activateContractWallet (Wallet 5) ownFunds'

    h3 <- activateContractWallet (Wallet 3) $ swap oracle
    h4 <- activateContractWallet (Wallet 4) $ swap oracle
    h5 <- activateContractWallet (Wallet 5) $ swap oracle

    callEndpoint @"offer" h3 10_000_000
    callEndpoint @"offer" h4 20_000_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 ()
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_700_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 ()
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_800_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"retrieve" h3 ()
    callEndpoint @"retrieve" h4 ()
    void $ Emulator.waitNSlots 3
  where
    getOracle :: ContractHandle (Last Oracle) OracleSchema Text -> EmulatorTrace Oracle
    getOracle h = do
        l <- observableState h
        case l of
            Last Nothing       -> Emulator.waitNSlots 1 >> getOracle h
            Last (Just oracle) -> Extras.logInfo (show oracle) >> return oracle
-}
