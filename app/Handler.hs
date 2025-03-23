{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
module Handler where 

import Prelude hiding (id, (.)) -- we get that from Control.Category

import Control.Concurrent.STM
import Control.Applicative

import Control.Category
import Control.Arrow

import Control.Lens

import Control.Exception (bracket)

data Handler i o = forall g r. HandlerWrapper {
    wrappedHandler :: Handler' g r i o
  }
data Handler' g r i o = HandlerInt {
    with_global :: forall a. (g -> IO a) -> IO a
  , with_local :: g -> forall a. (r -> IO a) -> IO a
  , handle :: g -> r -> i -> STM o
  }

eitherH :: Handler i o -> Handler i o -> Handler i o
eitherH = combineH (\a b i -> a i <|> b i)

composeH :: Handler i m -> Handler m o -> Handler i o
composeH = combineH (\a b i -> a i >>= b)

combineH :: ((i -> STM o) -> (i' -> STM o') -> (i'' -> STM o'')) -> Handler i o -> Handler i' o' -> Handler i'' o''
combineH w (HandlerWrapper handler) (HandlerWrapper handler') = HandlerWrapper $ combineH' w handler handler'

combineH' :: ((i -> STM o) -> (i' -> STM o') -> (i'' -> STM o'')) -> Handler' g r i o -> Handler' g' r' i' o' -> Handler' (g, g') (r, r') i'' o''
combineH' w handler handler' = HandlerInt with_global' with_local' handle' where
  with_global' c = with_global handler $ \g -> with_global handler' $ \g' -> c (g, g')
  with_local' (g, g') c = with_local handler g $ \p -> with_local handler' g' $ \p' -> c (p, p')
  handle' (g, g') (p, p') = w (handle handler g p) (handle handler' g' p')

liftSTM :: (i -> STM o) -> Handler i o
liftSTM f = HandlerWrapper $ HandlerInt ($ ()) (const ($ ())) (const $ const $ f)

instance Category Handler where
  h . h' = composeH h' h
  id = arr id
instance Arrow Handler where
  h *** h' = combineH (\a a' (i,i') -> (,) <$> a i <*> a' i') h h'
  arr f = liftSTM $ return . f

instance Profunctor Handler where
  dimap before after (HandlerWrapper h) = HandlerWrapper (h { handle = handle' }) where
    handle' g r i = fmap after $ handle h g r $ before i

instance Functor (Handler i) where
  fmap = rmap

instance Applicative (Handler i) where
  pure x = arr $ const x
  (<*>) = combineH (\a b i -> a i <*> b i)

instance Alternative (Handler i) where
  empty = liftSTM $ const empty
  (<|>) = eitherH

withResources :: IO g -> (g -> IO ()) -> (g -> IO r) -> (g -> r -> IO ()) -> Handler (g,r,i) o -> Handler i o
withResources acqG relG acqL relL (HandlerWrapper handler) = HandlerWrapper $ withResources' acqG relG acqL relL handler

withResourcesBrackets :: (forall a. (g -> IO a) -> IO a) -> (forall a. g -> ((r -> IO a) -> IO a)) -> Handler (g,r,i) o -> Handler i o
withResourcesBrackets withG withL (HandlerWrapper handler) = HandlerWrapper $ withResourcesBrackets' withG withL handler

withResourcesBrackets' :: (forall a. (g -> IO a) -> IO a) -> (forall a. g -> ((r -> IO a) -> IO a)) ->Handler' g' r' (g,r,i) o -> Handler' (g', g) (r', r) i o
withResourcesBrackets' withG withL (HandlerInt {..}) = HandlerInt with_global' with_local' handle' where
    with_global' cb = with_global $ \g' -> withG $ \g -> cb (g', g) -- >>= \g -> cb (g', g)
    with_local' (g', g) cb = with_local g' $ \r' -> withL g $ \r -> cb (r', r)
    handle' (g', g) (r', r) i = handle g' r' (g,r,i)

withResources' :: IO g -> (g -> IO ()) -> (g -> IO r) -> (g -> r -> IO ()) -> Handler' g' r' (g,r,i) o -> Handler' (g', g) (r', r) i o
withResources' acqG relG acqL relL (HandlerInt {..}) = HandlerInt with_global' with_local' handle' where
    with_global' cb = with_global $ \g' -> bracket acqG relG $ \g -> cb (g', g) -- >>= \g -> cb (g', g)
    with_local' (g', g) cb = with_local g' $ \r' -> bracket (acqL g) (relL g) $ \r -> cb (r', r)
    handle' (g', g) (r', r) i = handle g' r' (g,r,i)
