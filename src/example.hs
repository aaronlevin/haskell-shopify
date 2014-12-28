module Main where

import Control.Monad.Free (Free)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Proxy (Proxy(Proxy))
import Data.Text (pack)
import Network.API.Shopify.Client
import Network.API.Shopify.Types
import Network.HTTP.Client (withManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getArgs)

getProducts :: Free CrudF [Product]
getProducts = do
    eitherProductsOrFail <- readF SProducts Proxy
    return $ case eitherProductsOrFail of
        Left _ -> []
        Right ps -> ps

main :: IO ()
main = do
  (apiKey:password:_) <- getArgs
  let auth = BasicCred (pack apiKey) (pack password)
  products <- withManager tlsManagerSettings $ \mgr -> runReaderT (httpShopify getProducts) (mgr,auth)
  print products
