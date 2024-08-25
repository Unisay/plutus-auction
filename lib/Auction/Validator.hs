{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Auction.Validator where

import PlutusTx
import PlutusTx.Prelude

import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import GHC.Generics (Generic)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (lovelaceValue)
import PlutusLedgerApi.V3 (
  Datum (..),
  Lovelace,
  OutputDatum (..),
  POSIXTime,
  PubKeyHash,
  Redeemer (..),
  ScriptContext (..),
  ScriptInfo (SpendingScript),
  TxInfo (..),
  TxOut (..),
  Value,
  from,
  serialiseCompiledCode,
  to,
  txOutAddress,
 )
import PlutusLedgerApi.V3.Contexts (getContinuingOutputs)
import PlutusTx.AsData qualified as PlutusTx
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Show (Show (show))
import PlutusTx.Show qualified as PlutusTx
import Prelude qualified as Haskell

data AuctionParams = AuctionParams
  { apSeller :: PubKeyHash
  -- ^ Seller's wallet address. The highest bid (if exists) will be sent to the
  -- seller. If there is no bid, the asset auctioned will be sent to the seller.
  , apAsset :: Value
  -- ^ The asset being auctioned. It can be a single token, multiple tokens of
  -- the same kind, or tokens of different kinds, and the token(s) can be
  -- fungible or non-fungible.  These can all be encoded as a `Value`.
  , apMinBid :: Lovelace
  -- ^ The minimum bid in Lovelace.
  , apEndTime :: POSIXTime
  -- ^ The deadline for placing a bid. This is the earliest time the auction can
  -- be closed.
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

$(PlutusTx.makeIsDataSchemaIndexed ''AuctionParams [('AuctionParams, 0)])
$(PlutusTx.makeLift ''AuctionParams)

data Bid = Bid
  { bBidder :: PubKeyHash
  -- ^ Bidder's wallet address.
  , bAmount :: Lovelace
  -- ^ Bid amount in Lovelace.
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

$(PlutusTx.deriveShow ''Bid)
$(PlutusTx.makeIsDataSchemaIndexed ''Bid [('Bid, 0)])

instance Eq Bid where
  {-# INLINEABLE (==) #-}
  bid == bid' = bBidder bid == bBidder bid' && bAmount bid == bAmount bid'

{- | Datum represents the state of a smart contract. In this case
it contains the highest bid so far (if exists).
-}
newtype AuctionDatum = AuctionDatum {adHighestBid :: Maybe Bid}
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

$(PlutusTx.makeIsDataSchemaIndexed ''AuctionDatum [('AuctionDatum, 0)])

{- | Redeemer is the input that changes the state of a smart contract.
In this case it is either a new bid, or a request to close the auction
and pay out the seller and the highest bidder.
-}
data AuctionRedeemer = NewBid Bid | PayOut
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

$(PlutusTx.makeIsDataSchemaIndexed ''AuctionRedeemer [('NewBid, 0), ('PayOut, 1)])

{- | Given the auction parameters, determines whether the transaction is allowed
to spend the UTXO.
-}
{-# INLINEABLE auctionTypedValidator #-}
auctionTypedValidator
  :: AuctionParams
  -> AuctionDatum
  -> AuctionRedeemer
  -> ScriptContext
  -> Bool
auctionTypedValidator
  params
  (AuctionDatum highestBid)
  redeemer
  ctx@(ScriptContext txInfo _ _) = and conditions
   where
    conditions :: [Bool]
    conditions =
      case redeemer of
        NewBid bid ->
          [ sufficientBid bid
          , validBidTime
          , refundPreviouseHighestBid
          , correctNewDatum bid
          ]
        PayOut ->
          [ validPayoutTime
          , sellerGetsHighestBid
          , highestBidderGetsAsset
          ]

    sufficientBid :: Bid -> Bool
    sufficientBid (Bid _ amount) = case highestBid of
      Just (Bid _ amount') -> amount > amount'
      Nothing -> amount > apMinBid params

    validBidTime :: Bool
    validBidTime = to (apEndTime params) `contains` txInfoValidRange txInfo

    refundPreviouseHighestBid :: Bool
    refundPreviouseHighestBid = case highestBid of
      Nothing -> True
      Just (Bid bidder amount) -> case find
        ( \o ->
            txOutAddress o
              == pubKeyHashAddress bidder
              && txOutValue o
              == lovelaceValue amount
        )
        (txInfoOutputs txInfo) of
        Just _ -> True
        Nothing -> traceError "Not found: refund output"

    correctNewDatum :: Bid -> Bool
    correctNewDatum bid =
      case getContinuingOutputs ctx of
        [o] ->
          case txOutDatum o of
            OutputDatum (Datum newDatum) ->
              case fromBuiltinData newDatum of
                Just bid' ->
                  traceIfFalse
                    ( "Invalid output datum: expected "
                        <> show bid
                        <> ", but got "
                        <> show bid'
                    )
                    (bid == bid')
                Nothing ->
                  traceError ("Failed to decode output datum" <> show newDatum)
            OutputDatumHash _ ->
              traceError "Expected OutputDatum, got OutputDatumHash"
            NoOutputDatum ->
              traceError "Expected OutputDatum, got NoOutputDatum"
        os ->
          traceError
            ( "Expected exactly one continuing output, got "
                <> show (length os)
            )

    validPayoutTime :: Bool
    validPayoutTime = from (apEndTime params) `contains` txInfoValidRange txInfo

    sellerGetsHighestBid :: Bool
    sellerGetsHighestBid = case highestBid of
      Nothing -> True
      Just (Bid _ amount) -> case find
        ( \o ->
            txOutAddress o
              == pubKeyHashAddress (apSeller params)
              && txOutValue o
              == lovelaceValue amount
        )
        (txInfoOutputs txInfo) of
        Just _ -> True
        Nothing -> traceError "Not found: Output paid to seller"

    highestBidderGetsAsset :: Bool
    highestBidderGetsAsset =
      case highestBid of
        Nothing -> True
        Just (Bid bidder _) ->
          case find
            ( \o ->
                txOutAddress o
                  == pubKeyHashAddress bidder
                  && txOutValue o
                  == apAsset params
            )
            (txInfoOutputs txInfo) of
            Just _ -> True
            Nothing -> traceError "Not found: Output paid to highest bidder"

{-# INLINEABLE auctionUntypedValidator #-}
auctionUntypedValidator :: AuctionParams -> BuiltinData -> BuiltinUnit
auctionUntypedValidator params scriptContextBuiltinData =
  check (auctionTypedValidator params datum redeemer scriptContext)
 where
  (scriptContext, redeemer, datum) =
    case unsafeFromBuiltinData scriptContextBuiltinData of
      ctx@( ScriptContext
              _txInfo
              (Redeemer redeemerBuiltinData)
              (SpendingScript _txOutRef optionalDatum)
            ) ->
          ( ctx
          , unsafeFromBuiltinData redeemerBuiltinData
          , case optionalDatum of
              Nothing -> traceError "Expected datum, got Nothing"
              Just dm -> unsafeFromBuiltinData (getDatum dm)
          )
      _ -> traceError "Invalid ScriptContext"

PlutusTx.asData
  [d|
    data Bid' = Bid'
      { bBidder' :: PubKeyHash -- Bidder's wallet address.
      , bAmount' :: Lovelace -- Bid amount in Lovelace.
      }
      -- We can derive instances with the newtype strategy, and they
      -- will be based on the instances for 'Data'
      deriving newtype
        ( Haskell.Eq
        , Haskell.Ord
        , PlutusTx.ToData
        , FromData
        , UnsafeFromData
        )

    -- don't do this for the datum, since it's just a newtype so
    -- simply delegates to the underlying type

    -- Redeemer is the input that changes the state of a smart contract.
    -- In this case it is either a new bid, or a request to close the auction
    -- and pay out the seller and the highest bidder.
    data AuctionRedeemer' = NewBid' Bid | Payout'
      deriving newtype
        ( Haskell.Eq
        , Haskell.Ord
        , PlutusTx.ToData
        , FromData
        , UnsafeFromData
        )
    |]

validatorCode :: AuctionParams -> CompiledCode (BuiltinData -> BuiltinUnit)
validatorCode params =
  $$(compile [||auctionUntypedValidator||])
    `unsafeApplyCode` liftCode plcVersion100 params

validatorEncoded :: AuctionParams -> ByteString
validatorEncoded = fromShort . serialiseCompiledCode . validatorCode
