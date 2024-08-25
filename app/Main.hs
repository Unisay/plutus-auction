module Main where

import Prelude

import Auction.Blueprint qualified
import Auction.Validator (AuctionParams (..))
import Auction.Validator qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import PlutusLedgerApi.V1.Value (lovelaceValue)

main :: IO ()
main = do
  let validator = Auction.Validator.validatorEncoded auctionParams
  writeCompiledValidatorToFile validator
  writeBlueprintToFile validator

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

auctionParams :: AuctionParams
auctionParams =
  AuctionParams
    { apSeller = "A42366274383B5CEAC7CF9E1174DB43E"
    , apAsset = lovelaceValue 1000000
    , apMinBid = 100
    , apEndTime = 1000
    }
