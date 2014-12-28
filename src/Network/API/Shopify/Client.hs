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
import Data.Aeson (encode)
import Data.Conduit (ResumableSource)
import Data.Proxy (Proxy)
import Network.HTTP.Conduit (http, RequestBody (RequestBodyLBS))
import Network.HTTP.Client.Conduit (Manager, method, parseUrl, Response, Request(host, path,requestBody, requestHeaders), withManager)
import Network.API.Shopify.Request (
    createMetafieldReq
  , createProductReq
  , createVariantReq
  , deleteMetafieldReq
  , deleteProductReq
  , deleteVariantReq
  , readMetafieldReq
  , readProductReq
  , readProductsReq
  , readVariantReq
  , updateMetafieldReq
  , updateProductReq
  , updateVariantReq
  )
import Network.API.Shopify.Types (
    Metafield
  , MetafieldId
  , Product
  , ProductId
  , Variant
  , VariantId
  )
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
type family ReadData (c :: Crudable) :: * where
    ReadData 'MetafieldCRUD = MetafieldId
    ReadData 'ProductCRUD   = ProductId
    ReadData 'ProductsR     = Proxy Nothing
    ReadData 'VariantCRUD   = VariantId

-- | type family mapping: (Crudable Type) X (Interface) -> required data to create
type family UpdateData (c :: Crudable) :: * where
    UpdateData 'MetafieldCRUD = (MetafieldId, Metafield)
    UpdateData 'ProductCRUD   = (ProductId, Product)
    UpdateData 'VariantCRUD   = (VariantId, Variant)

-- | type family mapping: (Crudable Type) X (Interface) -> required data to create
type family DeleteData (c :: Crudable) :: * where
    DeleteData 'MetafieldCRUD = MetafieldId
    DeleteData 'ProductCRUD   = ProductId
    DeleteData 'VariantCRUD   = VariantId

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

-- | Method to create `Request` objects for create requests.
-- purpusefully non-exhuastive as we cannot "create" via the
-- Products endpoint. This is still safe as any attempt to match
-- on SProductsR will yield a type error as CreateData 'ProductsR
-- is not specified.
createRequest :: SCrudable c
              -> CreateData c
              -> OAuthToken
              -> Request
createRequest SMetafield d token =
    authorizeRequest token req
      where req = createMetafieldReq {
                requestBody = RequestBodyLBS body
            }
            body = encode d
createRequest SProduct d token =
    authorizeRequest token req
      where req = createProductReq {
                requestBody = RequestBodyLBS body
            }
            body = encode d
createRequest SVariant d token =
    authorizeRequest token req
      where req = createVariantReq {
                requestBody = RequestBodyLBS body
            }
            body = encode d

-- | method to create `Request` objects for read requests.
readRequest :: SCrudable c
            -> ReadData c
            -> OAuthToken
            -> Request
readRequest SMetafield d token =
    authorizeRequest token (readMetafieldReq d)
readRequest SProduct d token =
    authorizeRequest token (readProductReq d)
readRequest SProducts _ token =
    authorizeRequest token readProductsReq
readRequest SVariant d token =
    authorizeRequest token (readVariantReq d)

-- | method to create `Request` objects for update requests
updateRequest :: SCrudable c
              -> UpdateData c
              -> OAuthToken
              -> Request
updateRequest SMetafield (metafieldId,d) token =
    authorizeRequest token req
      where req = (updateMetafieldReq metafieldId) {
                requestBody = RequestBodyLBS body
            }
            body = encode d
updateRequest SProduct (productId,d) token =
    authorizeRequest token req
      where req = (updateProductReq productId) {
                requestBody = RequestBodyLBS body
            }
            body = encode d
updateRequest SVariant (variantId,d) token =
    authorizeRequest token req
      where req = (updateVariantReq variantId) {
                requestBody = RequestBodyLBS body
            }
            body = encode d

-- | method to create `Request` objects for delete requests
deleteRequest :: SCrudable c
              -> DeleteData c
              -> OAuthToken
              -> Request
deleteRequest SMetafield d token =
    authorizeRequest token (deleteMetafieldReq d)
deleteRequest SProduct d token =
    authorizeRequest token (deleteProductReq d)
deleteRequest SVariant d token =
    authorizeRequest token (deleteVariantReq d)


-- | HTTP interpreter
-- httpShopify :: (MonadResource m, MonadIO m)
            -- => Free CrudF a
            -- -> ReaderT (Manager, OAuthToken) m a
-- httpShopify (Pure a) = return a
-- httpShopify (Free (CreateF SMetafield d g)) = do
    -- (mgr,token) <- ask


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
