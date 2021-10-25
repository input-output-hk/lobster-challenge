import Cardano.Api                                 hiding (TxId)
import Data.String                                 (IsString (..))
import Ledger.Time                                 (POSIXTime (..))
import Ledger.Value                                (AssetClass (..))
import Prelude
import System.Environment                          (getArgs)

import Cardano.PlutusLobster.LobsterPolicies       (counterTokenName, nftTokenName)
import Cardano.PlutusLobster.LobsterDeadlineScript

main :: IO ()
main = do
    [nftSymbol, otherSymbol, seed', nameCount', deadline'] <- getArgs
    let seed      = read seed'
        nft       = AssetClass (fromString nftSymbol,   nftTokenName)
        counter   = AssetClass (fromString otherSymbol, counterTokenName)
        finished  = AssetClass (fromString otherSymbol, finishedTokenName)
        nameCount = read nameCount'
        deadline  = POSIXTime $ read deadline'
        ldp       = LobsterDeadlineParams
            { ldpSeed      = seed
            , ldpNFT       = nft
            , ldpCounter   = counter
            , ldpFinished  = finished
            , ldpNameCount = nameCount
            , ldpDeadline  = deadline
            }
        lobsterDeadlineFile = "lobster-deadline.plutus"
    print ldp
    print expectedDatumHash

    lobsterDeadlineResult <- writeFileTextEnvelope lobsterDeadlineFile Nothing $ apiLobsterDeadlineScript ldp
    case lobsterDeadlineResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote lobster-deadline script to file " ++ lobsterDeadlineFile
