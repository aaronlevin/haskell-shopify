{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Network.API.Shopify.Client (
  ) where

import Control.Monad.Free (Free(Free, Pure))
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (ResumableSource)
import Network.HTTP.Conduit (http)
import Network.HTTP.Client.Conduit (Manager, method, parseUrl, Response, Request(host, path,requestHeaders), withManager)
import Network.API.Shopify.Types (Metafield, Product, Variant)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

-- | crudable types
data Crudable = MetafieldCRUD
              | ProductCRUD
              | ProductsR
              | VariantCRUD

-- | singleton support
data SCrudable (c :: Crudable) :: * where
    SMetafield :: SCrudable 'MetafieldCRUD
    SProduct   :: SCrudable 'ProductCRUD
    SProducts  :: SCrudable 'ProductsR
    SVariant   :: SCrudable 'VariantCRUD

-- | type family that maps a Crudable type to its type
type family CrudBase (c :: Crudable) :: * where
    CrudBase 'MetafieldCRUD = Metafield
    CrudBase 'ProductCRUD   = Product
    CrudBase 'ProductsR     = [Product]
    CrudBase 'VariantCRUD   = Variant

-- | type family mapping: (Crudable Type) X (Interface) -> required data to create
type family CreateData (c :: Crudable) :: * where
    CreateData 'MetafieldCRUD = Metafield
    CreateData 'ProductCRUD   = Product
    CreateData 'VariantCRUD   = Variant

-- | type family mapping: (Crudable Type) X (Interface) -> required data to create
type family UpdateData (c :: Crudable) :: * where
    UpdateData 'MetafieldCRUD = Metafield
    UpdateData 'ProductCRUD   = Product
    UpdateData 'VariantCRUD   = Variant

-- | type family mapping: (Crudable Type) X (Interface) -> required data to create
type family ReadData (c :: Crudable) :: * where
    ReadData 'MetafieldCRUD = Int
    ReadData 'ProductCRUD   = Int
    ReadData 'VariantCRUD   = Int

-- | type family mapping: (Crudable Type) X (Interface) -> required data to create
type family DeleteData (c :: Crudable) :: * where
    DeleteData 'MetafieldCRUD = Int
    DeleteData 'ProductCRUD   = Int
    DeleteData 'VariantCRUD    = Int

-- | various types of errors
data ShopifyError = ErrorAuthorization
                  | ErrorNetwork

-- | CRUD Algebra
data CrudF a where
    CreateF :: SCrudable c
            -> CreateData c
            -> (Either ShopifyError (CrudBase c) -> a)
            -> CrudF a
    ReadF   :: SCrudable c
            -> ReadData c
            -> (Either ShopifyError (CrudBase c) -> a)
            -> CrudF a
    UpdateF :: SCrudable c
            -> UpdateData c
            -> (Either ShopifyError () -> a)
            -> CrudF a
    DeleteF :: SCrudable c
            -> DeleteData c
            -> (Either ShopifyError () -> a)
            -> CrudF a

instance Functor CrudF where
  fmap f (CreateF c d g) = CreateF c d (f . g)
  fmap f (ReadF   c d g) = ReadF   c d (f . g)
  fmap f (UpdateF c d g) = UpdateF c d (f . g)
  fmap f (DeleteF c d g) = DeleteF c d (f . g)

newtype OAuthToken = OAuthToken Text

-- | smart create constructor.
create :: SCrudable c
       -> CreateData c
       -> Free CrudF (Either ShopifyError (CrudBase c))
create c d = Free $ CreateF c d Pure

-- | smart read constructor.
read :: SCrudable c
     -> ReadData c
     -> Free CrudF (Either ShopifyError (CrudBase c))
read c d = Free $ ReadF c d Pure

-- | smart update constructor.
update :: SCrudable c
       -> UpdateData c
       -> Free CrudF (Either ShopifyError ())
update c d = Free $ UpdateF c d Pure

-- | smart delete constructor.
delete :: SCrudable c
       -> DeleteData c
       -> Free CrudF (Either ShopifyError ())
delete c d = Free $ DeleteF c d Pure

-- | HTTP interpreter
-- httpShopify :: (MonadResource m, MonadIO m)
           -- => Free CrudF a
           -- -> ReaderT Manager (ReaderT OAuthToken m) a
-- httpShopify (Pure a) = return a
-- httpShopify (Free (CrudF SHTTP SImage d g)) = do
    -- mgr <- ask
    -- req <- parseUrl

-- | simple method to add the access token header to a request.
authorizeRequest :: OAuthToken -> Request -> Request
authorizeRequest (OAuthToken token) req = req { requestHeaders = newHeaders }
    where newHeaders = ("X-Shopify-Access-Token", encodeUtf8 token) : requestHeaders req
