{-# LANGUAGE OverloadedStrings     #-}

import Cardano.Api                           hiding (TxId)
import Data.String                           (IsString (..))
import Ledger.Value                          (AssetClass (..), mpsSymbol)
import Prelude
import System.Environment                    (getArgs)

import Cardano.PlutusLobster.LobsterPoliciesV4
import Cardano.PlutusLobster.LobsterV4Script

main :: IO ()
main = do
    [nftSymbol, seed', nameCount', voteCount', fee'] <- getArgs
    let seed        = read seed'
        nft         = AssetClass (fromString nftSymbol,   nftTokenName)
        fee         = read fee'
        pp          = PolicyParams
                      { pCounterTokenName = counterTokenName
                      , pVotesTokenName = votesTokenName
                      , pFees = fee
                      , pRequestHash = requestValidatorHash nft
                      , pNFT = nft
                      }
        otherSymbol = mpsSymbol $ otherPolicyHash pp
        counterAC   = AssetClass (otherSymbol, counterTokenName)
        voteAC      = AssetClass (otherSymbol, votesTokenName)
        nameCount   = read nameCount'
        voteCount   = read voteCount'
        lp          = LobsterParams
                      { lpSeed      = seed
                      , lpNameCount = nameCount
                      , lpVoteCount = voteCount
                      , lpNFT       = nft
                      , lpCounter   = counterAC
                      , lpVotes     = voteAC
                      }
        lobsterFile = "scripts/lobster-v4.plutus"
        requestFile = "scripts/lobster-request-v4.plutus"
        policyFile  = "scripts/lobster-other-policy-v4.plutus"
    putStrLn "Lobster Parameters:"
    print lp
    putStrLn "Policy Parameters:"
    print pp

    lobsterResult <- writeFileTextEnvelope lobsterFile Nothing $ apiLobsterScript lp
    case lobsterResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "Lobster script written to file " ++ lobsterFile

    requestResult <- writeFileTextEnvelope requestFile Nothing $ apiRequestScript nft
    case requestResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "Request script written to file " ++ requestFile


    policyResult <- writeFileTextEnvelope policyFile Nothing $ apiOtherMintScript pp
    case policyResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "Policy script written script to file " ++ policyFile
