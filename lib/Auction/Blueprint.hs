module Auction.Blueprint (writeToFile) where

import PlutusTx.Blueprint
import Prelude

import Auction.Validator (
  AuctionDatum (..),
  AuctionParams (..),
  AuctionRedeemer (..),
 )
import Data.ByteString (ByteString)
import Data.Set qualified as Set

auctionContractBlueprint
  :: Maybe ByteString
  -> ContractBlueprint
auctionContractBlueprint validatorCompiledCode =
  MkContractBlueprint
    { contractId = Just "auction-contract"
    , contractPreamble =
        MkPreamble
          { preambleTitle = "Auction Contract"
          , preambleDescription = Just "A simple auction contract"
          , preambleVersion = "1.0.0"
          , preamblePlutusVersion = PlutusV3
          , preambleLicense = Just "MIT"
          }
    , contractValidators =
        Set.singleton $
          MkValidatorBlueprint
            { validatorTitle = "Auction Validator"
            , validatorDescription = Just "Auction validator"
            , validatorParameters =
                [ MkParameterBlueprint
                    { parameterTitle = Just "Auction Validator Parameters"
                    , parameterDescription = Just "Compile-time validator parameters"
                    , parameterPurpose = Set.singleton Spend
                    , parameterSchema = definitionRef @AuctionParams
                    }
                ]
            , validatorRedeemer =
                MkArgumentBlueprint
                  { argumentTitle = Just "Auction Redeemer"
                  , argumentDescription = Just "Auction redeemer"
                  , argumentPurpose = Set.fromList [Spend, Mint]
                  , argumentSchema = definitionRef @AuctionRedeemer
                  }
            , validatorDatum =
                Just
                  MkArgumentBlueprint
                    { argumentTitle = Just "Auction Datum"
                    , argumentDescription = Just "Auction datum"
                    , argumentPurpose = Set.singleton Spend
                    , argumentSchema = definitionRef @AuctionDatum
                    }
            , validatorCompiledCode
            }
    , contractDefinitions =
        deriveDefinitions @[AuctionParams, AuctionDatum, AuctionRedeemer]
    }

writeToFile :: FilePath -> ByteString -> IO ()
writeToFile path code =
  writeBlueprint path (auctionContractBlueprint (Just code))
