{-# LANGUAGE OverloadedStrings     #-}

import Cardano.Api                           hiding (TxId)
import Data.String                           (IsString (..))
import Ledger
import Ledger.Value                          (AssetClass (..), mpsSymbol)
import Prelude
import System.Environment                    (getArgs)

import Cardano.PlutusLobster.LobsterPolicies
import Cardano.PlutusLobster.LobsterScript

main :: IO ()
main = do
    [nftSymbol, seed', nameCount', deadline', pkBatcher', sFees', cFees'] <- getArgs
    let seed        = read seed'
        nft         = AssetClass (fromString nftSymbol, nftTokenName)
        deadline    = POSIXTime $ read deadline'
        sFees       = read sFees'
        cFees       = read cFees'
        tkSymbol    = mpsSymbol $ ticketPolicyHash nft
        pkBatcher   = parsePubKeyHash pkBatcher'
        rp          = RequestParams
                      { rpNFT = nft
                      , rTicketSymbol = tkSymbol
                      , rpBatcher = pkBatcher
                      , rEndHash = endValidatorHash nft
                      }
        pp          = PolicyParams
                      { pCounterTokenName = counterTokenName
                      , pFinishedTokenName = finishedTokenName
                      , pSubmitFees = sFees
                      , pCollectFees = cFees
                      , pRequestHash = requestValidatorHash rp
                      , pNFT = nft
                      , pDeadline = deadline
                      }
        otherSymbol = mpsSymbol $ otherPolicyHash pp
        counterAC   = AssetClass (otherSymbol, counterTokenName)
        finishedAC  = AssetClass (otherSymbol, finishedTokenName)
        nameCount   = read nameCount'
        lp          = LobsterParams
                      { lpSeed      = seed
                      , lpNFT       = nft
                      , lpCounter   = counterAC
                      , lpFinished  = finishedAC
                      , lpTicketSymbol = tkSymbol
                      , lpNameCount = nameCount
                      , lpDeadline  = deadline
                      , lpBatcher   = pkBatcher
                      }
        lobsterFile = "../scripts/lobster.plutus"
        requestFile = "../scripts/lobster-request.plutus"
        endFile     = "../scripts/lobster-end.plutus"
        policyFile  = "../scripts/lobster-other-policy.plutus"
        ticketFile  = "../scripts/lobster-ticket-policy.plutus"
    putStrLn "Lobster Parameters:"
    print lp
    putStrLn "Policy Parameters:"
    print pp

    lobsterResult <- writeFileTextEnvelope lobsterFile Nothing $ apiLobsterScript lp
    case lobsterResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "Lobster script written to file " ++ lobsterFile

    requestResult <- writeFileTextEnvelope requestFile Nothing $ apiRequestScript rp
    case requestResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "Request script written to file " ++ requestFile


    endResult <- writeFileTextEnvelope endFile Nothing $ apiEndScript nft
    case endResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "End script written to file " ++ endFile


    policyResult <- writeFileTextEnvelope policyFile Nothing $ apiOtherMintScript pp
    case policyResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "Policy script written script to file " ++ policyFile


    ticketResult <- writeFileTextEnvelope ticketFile Nothing $ apiTicketMintScript nft
    case ticketResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "Ticket Policy script written script to file " ++ ticketFile


parsePubKeyHash :: String -> PubKeyHash
parsePubKeyHash s = fromString s
