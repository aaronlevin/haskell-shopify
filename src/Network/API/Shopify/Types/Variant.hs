{-# LANGUAGE OverloadedStrings #-}

module Network.API.Shopify.Types.Variant(
    Variant(..)
  , VariantId(..)
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero)
import           Data.Aeson ( (.:)
                            , (.:?)
                            , (.=)
                            , FromJSON(parseJSON)
                            , object
                            , Value(Object)
                            , ToJSON(toJSON)
                            )
import qualified Data.Aeson as A
import           Data.Maybe (catMaybes)
import           Data.Scientific (fromFloatDigits, scientific)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Network.API.Shopify.Types.InventoryPolicy (InventoryPolicy)

newtype VariantId = VariantId Int

data Variant = Variant { variantBarcode :: Maybe Text
                       , variantCompareAtPrice :: Maybe Float
                       , variantCreatedAt :: UTCTime
                       , variantFulfillmentService :: Text
                       , variantGrams :: Int
                       , variantId :: VariantId
                       , variantInventoryManagement :: Maybe Text
                       , variantInventoryPolicy :: InventoryPolicy
                       , variantInventoryQuantity :: Int
                       , variantOldInventoryQuantity :: Int
                       , variantOption1 :: Maybe Text
                       , variantOption2 :: Maybe Text
                       , variantOption3 :: Maybe Text
                       , variantPosition :: Int
                       , variantPrice :: Float
                       , variantProductId :: Integer
                       , variantRequiresShipping :: Bool
                       , variantSku :: Maybe Text
                       , variantTaxable :: Bool
                       , variantTitle :: Text
                       , variantUpdatedAt :: UTCTime
                       , variantImageId :: Maybe Integer
                       }

emptyIsNothing :: Maybe Text -> Maybe Text
emptyIsNothing m = m >>= (\t -> if t == "" then Nothing else Just t)


instance FromJSON Variant where
  parseJSON(Object v) = Variant <$> (emptyIsNothing <$> v .:? "barcode")
                                <*> v .:? "compare_at_price"
                                <*> v .:  "created_at"
                                <*> v .:  "fulfillment_service"
                                <*> v .:  "grams"
                                <*> (VariantId <$> v .:  "id")
                                <*> v .:? "inventory_management"
                                <*> v .:  "inventory_policy"
                                <*> v .:  "inventory_quantity"
                                <*> v .:  "old_inventory_quantity"
                                <*> v .:  "option1"
                                <*> v .:  "option2"
                                <*> v .:  "option3"
                                <*> v .:  "position"
                                <*> v .:  "price"
                                <*> v .:  "product_id"
                                <*> v .:  "requires_shipping"
                                <*> (emptyIsNothing <$> v .:? "sku")
                                <*> v .:  "taxable"
                                <*> v .:  "title"
                                <*> v .:  "updated_at"
                                <*> v .:? "image_id"
  parseJSON _         = mzero

instance ToJSON Variant where
  toJSON (Variant barcode
                  compareAtPrice
                  createdAt
                  fulfillmentService
                  grams
                  (VariantId varId)
                  inventoryManagement
                  inventoryPolicy
                  inventoryQuantity
                  oldInventoryQuantity
                  opt1
                  opt2
                  opt3
                  position
                  price
                  productId
                  requiresShipping
                  sku
                  taxable
                  title
                  updatedAt
                  imageId
                  ) = object $ [ "created_at"             .= createdAt
                               , "fulfillment_service"    .= fulfillmentService
                               , "grams"                  .= grams
                               , "id"                     .= varId
                               , "inventory_policy"       .= inventoryPolicy
                               , "inventory_quantity"     .= inventoryQuantity
                               , "old_inventory_quantity" .= oldInventoryQuantity
                               , "position"               .= position
                               , "price"                  .= price
                               , "product_id"             .= productId
                               , "requires_shipping"      .= requiresShipping
                               , "taxable"                .= taxable
                               , "title"                  .= title
                               , "updated_at"             .= updatedAt
                               ] ++ catMaybes
                               [ ("barcode" .=)              . A.String <$> barcode
                               , ("compare_at_price" .=)     . A.Number . fromFloatDigits <$> compareAtPrice
                               , ("option1" .=)              . A.String <$> opt1
                               , ("option2" .=)              . A.String <$> opt2
                               , ("option3" .=)              . A.String <$> opt3
                               , ("inventory_management" .=) . A.String <$> inventoryManagement
                               , ("sku" .=)                  . A.String <$> sku
                               , ("image_id" .=)             . A.Number . flip scientific 0 <$> imageId
                               ]
