import Cardano.Api                                  hiding (TxId)
import Data.String                                  (IsString (..))
import Ledger.Value                                 (AssetClass (..))
import Prelude
import System.Environment                           (getArgs)

import Cardano.PlutusLobster.LobsterPolicies        (counterTokenName, nftTokenName)
import Cardano.PlutusLobster.LobsterVoteCountScript

main :: IO ()
main = do
    [nftSymbol, otherSymbol, seed', nameCount', voteCount'] <- getArgs
    let seed      = read seed'
        nft       = AssetClass (fromString nftSymbol,   nftTokenName)
        counter   = AssetClass (fromString otherSymbol, counterTokenName)
        votes     = AssetClass (fromString otherSymbol, votesTokenName)
        nameCount = read nameCount'
        voteCount = read voteCount'
        lvcp      = LobsterVoteCountParams
            { lvcpSeed      = seed
            , lvcpNFT       = nft
            , lvcpCounter   = counter
            , lvcpVotes     = votes
            , lvcpNameCount = nameCount
            , lvcpVoteCount = voteCount
            }
        lobsterVoteCountFile = "scripts/lobster-vote-count.plutus"
    print lvcp

    lobsterVoteCountResult <- writeFileTextEnvelope lobsterVoteCountFile Nothing $ apiLobsterVoteCountScript lvcp
    case lobsterVoteCountResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote lobster-vote-count script to file " ++ lobsterVoteCountFile
