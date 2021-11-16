import Cardano.Api                           hiding (TxId)
import Data.String                           (IsString (..))
import Ledger.Value                          (AssetClass (..))
import Prelude
import System.Environment                    (getArgs)

import Cardano.PlutusLobster.LobsterPolicies
import Cardano.PlutusLobster.LobsterV2Script

main :: IO ()
main = do
    [nftSymbol, otherSymbol, seed', nameCount', voteCount', fee'] <- getArgs
    let seed      = read seed'
        nft       = AssetClass (fromString nftSymbol,   nftTokenName)
        counter   = AssetClass (fromString otherSymbol, counterTokenName)
        votes     = AssetClass (fromString otherSymbol, votesTokenName)
        nameCount = read nameCount'
        voteCount = read voteCount'
        fee       = read fee'
        lp          = LobsterParams
            { lpSeed      = seed
            , lpNFT       = nft
            , lpCounter   = counter
            , lpVotes     = votes
            , lpNameCount = nameCount
            , lpVoteCount = voteCount
            , lpFee       = fee
            }
        lobsterFile = "scripts/lobster-v2.plutus"
        requestFile = "scripts/lobster-request.plutus"
    print lp

    lobsterResult <- writeFileTextEnvelope lobsterFile Nothing $ apiLobsterScript lp
    case lobsterResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote lobster script to file " ++ lobsterFile

    requestResult <- writeFileTextEnvelope requestFile Nothing $ apiRequestScript nft
    case requestResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote request script to file " ++ requestFile
