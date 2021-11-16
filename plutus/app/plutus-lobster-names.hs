{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad        (forM_)
import           Data.Aeson
import           Data.Aeson.Types     (parse)
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable        (toList)
import           System.IO
import           Text.Printf          (printf)

import           Paths_plutus_lobster (getDataFileName)

main :: IO ()
main = do
    ns <- map getName <$> readNames
    withFile "names.md" WriteMode $ \h -> do
        hPutStrLn h "# List of Names"
        hPutStrLn h "| Index | Name         |"
        hPutStrLn h "| -----:| ------------ |"

        forM_ (zip [(0 :: Int) ..] ns) $
            hPutStr h . uncurry (printf "| %4d  | %-12s |\n")

readNames :: IO [Value]
readNames = do
    f  <- getDataFileName "ENGivenMale.json"
    bs <-BSL.readFile f
    case decode bs of
        Just (Array vs) -> return $ toList vs
        _               -> ioError $ userError "invalid JSON"

getName :: Value -> String
getName (Object obj) = case parse (.: "name") obj of
    Error e   -> error e
    Success s -> s
getName _            = error "object expected"
