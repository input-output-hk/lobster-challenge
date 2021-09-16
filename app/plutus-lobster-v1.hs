import Cardano.Api                           hiding (TxId)
import Data.String                           (IsString (..))
import Ledger.Value                          (AssetClass (..))
import Prelude
import System.Environment                    (getArgs)

import Cardano.PlutusLobster.LobsterPolicies
import Cardano.PlutusLobster.LobsterV1Script

main :: IO ()
main = do
    [nftSymbol, otherSymbol, seed', nameCount', voteCount'] <- getArgs
    let seed      = read seed'
        nft       = AssetClass (fromString nftSymbol,   nftTokenName)
        counter   = AssetClass (fromString otherSymbol, counterTokenName)
        votes     = AssetClass (fromString otherSymbol, votesTokenName)
        nameCount = read nameCount'
        voteCount = read voteCount'
        lp          = LobsterParams
            { lpSeed      = seed
            , lpNFT       = nft
            , lpCounter   = counter
            , lpVotes     = votes
            , lpNameCount = nameCount
            , lpVoteCount = voteCount
            }
        lobsterFile = "scripts/lobster.plutus"
    print lp

    lobsterResult <- writeFileTextEnvelope lobsterFile Nothing $ apiLobsterScript lp
    case lobsterResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote lobster script to file " ++ lobsterFile
