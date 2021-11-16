
import Cardano.Api                           hiding (TxId)
import Cardano.CLI.Shelley.Script            as CScript
import Control.Monad.Trans.Except.Extra      (handleIOExceptT, runExceptT)
import qualified Data.ByteString.Char8       as BS
import Data.String                           (IsString (..))
import Ledger.Value                          (AssetClass (..), mpsSymbol)
import Prelude
import System.Environment                    (getArgs)
import PlutusTx.Builtins                     as Builtins
import Cardano.PlutusLobster.LobsterPoliciesV4
import Cardano.PlutusLobster.LobsterV4Script


main :: IO ()
main = do
    [nftSymbol, seed', nameCount', voteCount', fee'] <- getArgs
    let seed        = read seed'
        nft         = AssetClass (fromString nftSymbol, nftTokenName)
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

    -- lobsterResult <- writeFileTextEnvelope lobsterFile Nothing $ apiLobsterScript lp
    -- case lobsterResult of
    --     Left err -> print $ displayError err
    --     Right () -> putStrLn $ "Lobster script written to file " ++ lobsterFile

    -- requestResult <- writeFileTextEnvelope requestFile Nothing $ apiRequestScript nft
    -- case requestResult of
    --     Left err -> print $ displayError err
    --     Right () -> putStrLn $ "Request script written to file " ++ requestFile


    -- policyResult <- writeFileTextEnvelope policyFile Nothing $ apiOtherMintScript pp
    -- case policyResult of
    --     Left err -> print $ displayError err
    --     Right () -> putStrLn $ "Policy script written script to file " ++ policyFile


    putStrLn "Decoding script as performed by CLI to generate address !!!\n"
    hd <- runExceptT $ handleIOExceptT (FileIOError requestFile) $ BS.readFile requestFile
    case hd of
      Right scriptBytes ->
        case CScript.deserialiseScriptInAnyLang scriptBytes of
          Left _ -> putStrLn "Unable to decode script !!!"
          Right (ScriptInAnyLang _ script) -> do
            let bs = Builtins.toBuiltin $ serialiseToRawBytes $ hashScript script
                h = requestValidatorHash nft
            putStrLn $  "hash from file:" ++ show bs
            putStrLn $ "hash from validatorHash:" ++ show h ++ " nft:" ++ show nft

      _ -> putStrLn "Unable to read script file !!!"
