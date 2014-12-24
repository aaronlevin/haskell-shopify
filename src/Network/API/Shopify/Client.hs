{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Network.API.Shopify.Client (
  ) where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Data.Conduit (ResumableSource)
import Data.Proxy (Proxy(Proxy))
import Network.HTTP.Conduit (http)
import Network.HTTP.Client.Conduit (Manager, method, Response, Request(requestHeaders), withManager)
import Network.API.Shopify.Types (Product)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

newtype OAuthToken = OAuthToken Text

-- | simple method to add the access token header to a request.
authorizeRequest :: OAuthToken -> Request -> Request
authorizeRequest (OAuthToken token) req = req { requestHeaders = newHeaders }
    where newHeaders = ("X-Shopify-Access-Token", encodeUtf8 token) : (requestHeaders req)
