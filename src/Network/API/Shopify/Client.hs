{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Network.API.Shopify.Client (
    createF
  , CrudF(..)
  , deleteF
  , httpShopify
  , readF
  , SCrudable(..)
  , updateF
  ) where

import Control.Monad.Free (Free(Free, Pure))
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Reader (ask, ReaderT)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy)
import Network.HTTP.Conduit (httpLbs, RequestBody (RequestBodyLBS), Response(responseBody, responseStatus))
import Network.HTTP.Client.Conduit (Manager, Request(requestBody))
import Network.HTTP.Types.Status (Status(Status))
import Network.API.Shopify.Request (
    authorizeRequest
  , createMetafieldReq
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
  , APICredential
  , Product
  , ProductId
  , ShopifyError(ErrorResponseBodyNotParseable, ErrorHTTPResponseCode)
  , StoreName
  , Variant
  , VariantId
  )

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
    ReadData 'ProductsR     = Proxy ()
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


-- | smart create constructor.
createF :: SCrudable c
        -> CreateData c
        -> Free CrudF (Either ShopifyError (CrudBase c))
createF c d = Free $ CreateF c d Pure

-- | smart read constructor.
readF :: SCrudable c
      -> ReadData c
      -> Free CrudF (Either ShopifyError (CrudBase c))
readF c d = Free $ ReadF c d Pure

-- | smart update constructor.
updateF :: SCrudable c
        -> UpdateData c
        -> Free CrudF (Either ShopifyError ())
updateF c d = Free $ UpdateF c d Pure

-- | smart delete constructor.
deleteF :: SCrudable c
        -> DeleteData c
        -> Free CrudF (Either ShopifyError ())
deleteF c d = Free $ DeleteF c d Pure

-- | Method to create `Request` objects for create requests.
-- purpusefully non-exhuastive as we cannot "create" via the
-- Products endpoint. This is still safe as any attempt to match
-- on SProductsR will yield a type error as CreateData 'ProductsR
-- is not specified.
createRequest :: SCrudable c
              -> CreateData c
              -> StoreName
              -> APICredential
              -> Request
createRequest SMetafield d storeName cred =
    authorizeRequest cred req
      where req = (createMetafieldReq storeName) {
                requestBody = RequestBodyLBS body
            }
            body = encode d
createRequest SProduct d storeName cred =
    authorizeRequest cred req
      where req = (createProductReq storeName) {
                requestBody = RequestBodyLBS body
            }
            body = encode d
createRequest SVariant d storeName cred =
    authorizeRequest cred req
      where req = (createVariantReq storeName) {
                requestBody = RequestBodyLBS body
            }
            body = encode d

-- | method to create `Request` objects for read requests.
readRequest :: SCrudable c
            -> ReadData c
            -> StoreName
            -> APICredential
            -> Request
readRequest SMetafield d storeName cred =
    authorizeRequest cred (readMetafieldReq storeName d)
readRequest SProduct d storeName cred =
    authorizeRequest cred (readProductReq storeName d)
readRequest SProducts _ storeName cred =
    authorizeRequest cred (readProductsReq storeName)
readRequest SVariant d storeName cred =
    authorizeRequest cred (readVariantReq storeName d)

-- | method to create `Request` objects for update requests
updateRequest :: SCrudable c
              -> UpdateData c
              -> StoreName
              -> APICredential
              -> Request
updateRequest SMetafield (metafieldId,d) storeName cred =
    authorizeRequest cred req
      where req = (updateMetafieldReq storeName metafieldId) {
                requestBody = RequestBodyLBS body
            }
            body = encode d
updateRequest SProduct (productId,d) storeName cred =
    authorizeRequest cred req
      where req = (updateProductReq storeName productId) {
                requestBody = RequestBodyLBS body
            }
            body = encode d
updateRequest SVariant (variantId,d) storeName cred =
    authorizeRequest cred req
      where req = (updateVariantReq storeName variantId) {
                requestBody = RequestBodyLBS body
            }
            body = encode d

-- | method to create `Request` objects for delete requests
deleteRequest :: SCrudable c
              -> DeleteData c
              -> StoreName
              -> APICredential
              -> Request
deleteRequest SMetafield d storeName cred =
    authorizeRequest cred (deleteMetafieldReq storeName d)
deleteRequest SProduct d storeName cred =
    authorizeRequest cred (deleteProductReq storeName d)
deleteRequest SVariant d storeName cred =
    authorizeRequest cred (deleteVariantReq storeName d)

-- | handle response codes.
checkResponse :: Response b -> Either ShopifyError ()
checkResponse resp = case responseStatus resp of
    (Status 200 _) -> Right ()
    (Status 201 _) -> Right ()
    (Status 202 _) -> Right ()
    (Status 203 _) -> Right ()
    (Status 204 _) -> Right ()
    (Status 205 _) -> Right ()
    (Status 206 _) -> Right ()
    _              -> Left ErrorHTTPResponseCode

-- | helper method to handle http responses
-- for type inference reasons, we need to pattern match on
-- SCrudable type.
decodeResponse :: SCrudable c
               -> Response ByteString
               -> Either ShopifyError (CrudBase c)
decodeResponse SMetafield resp = checkResponse resp >>= \_ -> case decode (responseBody resp) of
    Nothing  -> Left ErrorResponseBodyNotParseable
    Just val -> Right val
decodeResponse SProduct resp = checkResponse resp >>= \_ -> case decode (responseBody resp) of
    Nothing  -> Left ErrorResponseBodyNotParseable
    Just val -> Right val
decodeResponse SProducts resp = checkResponse resp >>= \_ -> case decode (responseBody resp) of
    Nothing  -> Left ErrorResponseBodyNotParseable
    Just val -> Right val
decodeResponse SVariant resp = checkResponse resp >>= \_ -> case decode (responseBody resp) of
    Nothing  -> Left ErrorResponseBodyNotParseable
    Just val -> Right val

-- | HTTP interpreter
-- TODO: error handling
httpShopify :: (MonadIO m)
            => Free CrudF a
            -> ReaderT (Manager, StoreName, APICredential) m a
httpShopify (Pure a) = return a
httpShopify (Free (CreateF s d g)) = do
    (mgr, storeName, cred) <- ask
    response <- httpLbs (createRequest s d storeName cred) mgr
    httpShopify . g . decodeResponse s $ response
httpShopify (Free (ReadF s d g)) = do
    (mgr, storeName, cred) <- ask
    response <- httpLbs (readRequest s d storeName cred) mgr
    httpShopify . g . decodeResponse s $ response
httpShopify (Free (UpdateF s d g)) = do
    (mgr, storeName, cred) <- ask
    response <- httpLbs (updateRequest s d storeName cred) mgr
    httpShopify . g . checkResponse $ response
httpShopify (Free (DeleteF s d g)) = do
    (mgr, storeName, cred) <- ask
    response <- httpLbs (deleteRequest s d storeName cred) mgr
    httpShopify . g . checkResponse $ response
