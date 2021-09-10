import Cardano.Api                         hiding (TxId)
import Data.String                         (IsString (..))
import Ledger
import Ledger.Value                        (AssetClass (..))
import Prelude
import System.Environment                  (getArgs)

import Cardano.PlutusLobster.LobsterScript

main :: IO ()
main = do
    [nftSymbol, otherSymbol, seed', deadline', nameCount'] <- getArgs
    let seed        = read seed'
        deadline    = POSIXTime $ read deadline'
        nft         = AssetClass (fromString nftSymbol, nftTokenName)
        counter     = AssetClass (fromString otherSymbol, counterTokenName)
        finished    = AssetClass (fromString otherSymbol, finishedTokenName)
        nameCount   = read nameCount'
        lp          = LobsterParams
            { lpSeed      = seed
            , lpNFT       = nft
            , lpCounter   = counter
            , lpFinished  = finished
            , lpDeadline  = deadline
            , lpNameCount = nameCount
            }
        lobsterFile = "lobster.plutus"
    print lp

    lobsterResult <- writeFileTextEnvelope lobsterFile Nothing $ apiLobsterScript lp
    case lobsterResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote lobster script to file " ++ lobsterFile
