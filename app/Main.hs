module Main where

import Prelude

import Auction.Blueprint qualified
import Auction.Validator (validatorEncoded)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

main :: IO ()
main = do
  writeCompiledValidatorToFile validatorEncoded
  writeBlueprintToFile validatorEncoded

writeCompiledValidatorToFile :: ByteString -> IO ()
writeCompiledValidatorToFile code = do
  let path = "validator/validator.uplc"
  putStrLn $ "Writing UPLC-compiled auction validator script to " ++ path
  BS.writeFile path code

writeBlueprintToFile :: ByteString -> IO ()
writeBlueprintToFile code = do
  let path = "validator/validator.json"
  putStrLn $ "Writing auction contract blueprint to " ++ path
  Auction.Blueprint.writeToFile path code
