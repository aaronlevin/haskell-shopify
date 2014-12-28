module Network.API.Shopify.Types.ShopifyError(
  ShopifyError(..)
  ) where

-- | various types of errors
data ShopifyError = ErrorAuthorization
                  | ErrorNetwork
                  | ErrorResponseBodyNotParseable
                  | ErrorHTTPResponseCode
