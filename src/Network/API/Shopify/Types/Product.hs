{-# LANGUAGE OverloadedStrings #-}

module Network.API.Shopify.Types.Product (
    Product( .. )
  , ProductId( .. )
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero)
import           Data.Aeson ((.:), (.:?), (.=), FromJSON(parseJSON), object, ToJSON(toJSON), Value(Object))
import qualified Data.Aeson as A
import           Data.Maybe (catMaybes)
import           Data.Text (intercalate, splitOn, Text)
import           Data.Time.Clock (UTCTime)
import           Network.API.Shopify.Types.Image (Image)
import           Network.API.Shopify.Types.Option (Option)
import           Network.API.Shopify.Types.Variant (Variant)

newtype ProductId = ProductId Int deriving (Eq, Ord, Show)

data Product = Product { productBodyHtml :: Text
                       , productCreatedAt :: UTCTime
                       , productHandle :: Text
                       , productId :: ProductId
                       , productImages :: [Image]
                       , productOptions :: [Option]
                       , productType :: Text
                       , productPublishedAt :: UTCTime
                       , productPublishedScope :: Text
                       , productTags :: [Text]
                       , productTemplateSuffix :: Maybe Text
                       , productTitle :: Text
                       , productUpdatedAt :: UTCTime
                       , productVariants :: [Variant]
                       , productVendor :: Text
                       } 
                       deriving (Eq, Ord, Show)

splitAndFilter :: Text -> [Text]
splitAndFilter t = filter (/= "") $ splitOn "," t

instance FromJSON Product where
  parseJSON (Object v) = Product <$> v .: "body_html"
                                 <*> v .: "created_at"
                                 <*> v .: "handle"
                                 <*> (ProductId <$> v .: "id")
                                 <*> v .: "images"
                                 <*> v .: "options"
                                 <*> v .: "product_type"
                                 <*> v .: "published_at"
                                 <*> v .: "published_scope"
                                 <*> (splitAndFilter <$> v .: "tags")
                                 <*> v .:? "template_Suffix"
                                 <*> v .: "title"
                                 <*> v .: "updated_at"
                                 <*> v .: "variants"
                                 <*> v .: "vendor"
  parseJSON _          = mzero

instance ToJSON Product where
  toJSON (Product pBody
                  pCreatedAt
                  pHandle
                  (ProductId pId)
                  pImages
                  pOptions
                  pType
                  pPublishedAt
                  pPublishedScope
                  pTags
                  pTemplateSuffix
                  pTitle
                  pUpdatedAt
                  pVariants
                  pVendor
                  ) = object $ [ "body_html"       .= pBody 
                               , "created_at"      .= pCreatedAt
                               , "handle"          .= pHandle
                               , "id"              .= pId
                               , "images"          .= pImages
                               , "options"         .= pOptions
                               , "product_type"    .= pType
                               , "published_at"    .= pPublishedAt
                               , "published_scope" .= pPublishedScope
                               , "tags"            .= intercalate "," pTags
                               , "title"           .= pTitle
                               , "updated_at"      .= pUpdatedAt
                               , "variants"        .= pVariants
                               , "vendor"          .= pVendor
                               ] ++ catMaybes
                               [ ("template_suffix" .=) . A.String <$> pTemplateSuffix ]
