import Cardano.Api                         hiding (TxId)
import Data.String                         (IsString (..))
import Ledger
import Ledger.Bytes                        (getLedgerBytes)
import Prelude
import System.Environment                  (getArgs)

import Cardano.PlutusLobster.LobsterScript

main :: IO ()
main = do
    [utxo'] <- getArgs
    let utxo            = parseUTxO utxo'
        nftPolicyFile   = "nft-mint-policy.plutus"
        otherPolicyFile = "other-mint-policy.plutus"

    nftPolicyResult <- writeFileTextEnvelope nftPolicyFile Nothing $ apiNFTMintScript utxo
    case nftPolicyResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote NFT policy to file " ++ nftPolicyFile

    otherPolicyResult <- writeFileTextEnvelope otherPolicyFile Nothing apiOtherMintScript
    case otherPolicyResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote other policy to file " ++ otherPolicyFile

parseUTxO :: String -> TxOutRef
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    TxOutRef (TxId $ getLedgerBytes $ fromString x) $ read $ tail y

