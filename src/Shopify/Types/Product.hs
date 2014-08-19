module Shopify.Types.Product(
  Product( .. )
  ) where

-- import Control.Monad (mzero)
-- import Data.Aeson ((.:), (.:?), (.=), FromJSON(..), object, ToJSON(..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Shopify.Types (Image, Option)

data Product = Product { productBodyHtml :: Text
                       , productCreatedAt :: UTCTime
                       , productHandle :: Text
                       , productId :: Integer
                       , productImages :: [Image]
                       , productOptions :: [Option]
                       , productType :: Text
                       , productPublishedAt :: UTCTime
                       , productPublishedScope :: Text
                       , productTags :: [Text]
                       , productTemplateSuffix :: Text
                       , productTitle :: Text
                       , productUpdatedAt :: UTCTime
                       , productVariants :: [Text]
                       , productVendor :: Text
                       } 

-- instance FromJSON Product where
  -- parseJSON(Object v) = 
  -- parseJSON _ = mzero
