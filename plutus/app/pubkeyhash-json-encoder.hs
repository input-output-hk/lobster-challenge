{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Api                           as C
import           Cardano.Api.Shelley                   as C
import           Data.Aeson                            (encodeFile)
import           Data.String
import           Ledger
import qualified Plutus.V1.Ledger.Api                  as Api
import qualified PlutusTx
import           Prelude
import           Text.Printf                 (printf)
import           System.Environment                    (getArgs)

main :: IO()
main = do
  [pkh', js_file] <- getArgs
  let pkh = parsePubKeyHash pkh'
      p_data = PlutusTx.toBuiltinData $ pkh
      s_data = (C.fromPlutusData . Api.builtinDataToData) p_data
      json = scriptDataToJson ScriptDataJsonDetailedSchema s_data
  putStrLn $ printf "Public Key Hash %s" $ show pkh
  encodeFile js_file json
  putStrLn $ printf "Public Key Hash Json encoding written to %s" js_file


parsePubKeyHash :: String -> PubKeyHash
parsePubKeyHash s = fromString s
