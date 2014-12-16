{-# LANGUAGE DeriveFunctor, Rank2Types, OverloadedStrings #-}

module Network.API.Shopify.Client (
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask, lift, ReaderT)
import Control.Monad.Free (Free(Free,Pure))
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Data.Conduit (ResumableSource)
import Data.IORef (IORef, readIORef)
import Network.HTTP.Conduit (http)
import Network.HTTP.Client.Conduit (Manager, method, Response, Request, withManager)
import Network.API.Shopify.Types (Product)
import Data.Text (Text)

data HTTPAlgebra req resp next = GET  req (resp -> next)
                               | POST req (resp -> next)
                               | PUT  req (resp -> next)
                               | DELETE req (resp -> next)
                               deriving Functor

threadF :: (Functor f, Functor g) => (forall b. f b -> g b) -> Free f a -> Free g a
threadF _ (Pure x)  = Pure x
threadF t (Free fa) = Free(t $ fmap (threadF t) fa)

getC :: (Functor f) 
     => (forall a. HTTPAlgebra req res a -> f a)
     -> req
     -> Free f res
getC f req = threadF f (Free(GET req Pure))

postC :: (Functor f)
      => (forall a. HTTPAlgebra req res a -> f a)
      -> req
      -> Free f res
postC f req = threadF f (Free(POST req Pure))

type OAuthToken = IORef Text

getAndSetReqMethod :: HTTPAlgebra Request a b -> Request
getAndSetReqMethod (GET req _) = req { method = "GET" }
getAndSetReqMethod (POST req _) = req { method = "POST" }
getAndSetReqMethod (PUT req _) = req { method = "PUT" }
getAndSetReqMethod (DELETE req _) = req { method = "DELETE" }

getNextHttpAction :: HTTPAlgebra req resp next -> (resp -> next)
getNextHttpAction (GET _ f)    = f
getNextHttpAction (POST _ f)   = f
getNextHttpAction (PUT _ f)    = f
getNextHttpAction (DELETE _ f) = f

-- http-conduit interpreter with OAuth2 token dependency
httpConduitOAuth2I :: (MonadResource m, MonadIO m)
                   => HTTPAlgebra Request (Response (ResumableSource (ReaderT OAuthToken m) ByteString)) a
                   -> ReaderT Manager (ReaderT OAuthToken m) a
httpConduitOAuth2I httpAlg = do
  mgr <- ask
  let req = getAndSetReqMethod httpAlg
  let next = getNextHttpAction httpAlg
  lift $ do
    tokenRef <- ask
    oauth <- lift $ liftIO $ readIORef tokenRef
    next <$> http req mgr

-- within the context of any authentication we need to mutate the request param.
-- is this the responsibility of the interpreter? i.e. if it is, it'd be nice
-- to do this irrespective of the http client, as the semantics of creating
-- and mutating requests is an implementation detail.
-- type of `auth` = Reader Credentials

data ShopifyAlgebra e a = GetProduct Integer (Either e Product -> a)
                        | UpdateProduct Integer Product (Either e () -> a)
                        | CreateProduct Integer String String (Either e Product -> a)


data CRUDAlg a ident create error next = Create create (Either error a -> next)
                                       | Read ident (Either error a -> next)
                                       | Update ident a (Either error a -> next)
                                       | Delete ident (Either error () -> next)
                                       deriving Functor

type ProductCRUD a = CRUDAlg Product Integer String String a

createC :: (Functor f)
        => (forall b. CRUDAlg a ident create error b -> f b)
        -> create
        -> Free f (Either error a)
createC f create = threadF f (Free (Create create Pure))

readC :: (Functor f)
      => (forall b. CRUDAlg a ident create error b -> f b)
      -> ident
      -> Free f (Either error a)
readC f ident = threadF f (Free (Read ident Pure))
