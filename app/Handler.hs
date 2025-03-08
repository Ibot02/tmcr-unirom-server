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

data Handler i o = forall g r. HandlerWrapper {
    wrappedHandler :: Handler' g r i o
  }
data Handler' g r i o = HandlerInt {
    global_prepare :: forall a. (g -> IO a) -> IO a
  , prepare :: g -> forall a. (r -> IO a) -> IO a
  , handle :: g -> r -> i -> STM o
  , cleanup :: g -> r -> IO ()
  , cleanup_global :: g -> IO ()
  }

eitherH :: Handler i o -> Handler i o -> Handler i o
eitherH = combineH (\a b i -> a i <|> b i)

composeH :: Handler i m -> Handler m o -> Handler i o
composeH = combineH (\a b i -> a i >>= b)

combineH :: ((i -> STM o) -> (i' -> STM o') -> (i'' -> STM o'')) -> Handler i o -> Handler i' o' -> Handler i'' o''
combineH w (HandlerWrapper handler) (HandlerWrapper handler') = HandlerWrapper $ combineH' w handler handler'

combineH' :: ((i -> STM o) -> (i' -> STM o') -> (i'' -> STM o'')) -> Handler' g r i o -> Handler' g' r' i' o' -> Handler' (g, g') (r, r') i'' o''
combineH' w handler handler' = HandlerInt global_prepare' prepare' handle' cleanup' cleanup_global' where
  global_prepare' c = global_prepare handler $ \g -> global_prepare handler' $ \g' -> c (g, g')
  prepare' (g, g') c = prepare handler g $ \p -> prepare handler' g' $ \p' -> c (p, p')
  handle' (g, g') (p, p') = w (handle handler g p) (handle handler' g' p')
  cleanup' (g, g') (p, p') = cleanup handler g p >> cleanup handler' g' p'
  cleanup_global' (g, g') = cleanup_global handler g >> cleanup_global handler' g'

liftSTM :: (i -> STM o) -> Handler i o
liftSTM f = HandlerWrapper (HandlerInt ($ ()) (const ($ ())) (const $ const $ f) (const return) return )

instance Category Handler where
  h . h' = composeH h' h
  id = arr id
instance Arrow Handler where
  h *** h' = combineH (\a a' (i,i') -> (,) <$> a i <*> a' i') h h'
  arr f = liftSTM $ return . f

instance Profunctor Handler where
  dimap before after (HandlerWrapper h) = HandlerWrapper (h { handle = handle' }) where
    handle' g r i = fmap after $ handle h g r $ before i

withGlobal :: IO g -> (g -> IO ()) -> Handler (g,i) o -> Handler i o
withGlobal acq rel h = withResources acq rel (const $ return ()) (const return) $ lmap (\(g,(),i) -> (g, i)) h

withResources :: IO g -> (g -> IO ()) -> (g -> IO r) -> (g -> r -> IO ()) -> Handler (g,r,i) o -> Handler i o
withResources acqG relG acqL relL (HandlerWrapper handler) = HandlerWrapper $ withResources' acqG relG acqL relL handler

withResources' :: IO g -> (g -> IO ()) -> (g -> IO r) -> (g -> r -> IO ()) -> Handler' g' r' (g,r,i) o -> Handler' (g', g) (r', r) i o
withResources' acqG relG acqL relL (HandlerInt {..}) = HandlerInt global_prepare' prepare' handle' cleanup' cleanup_global' where
    global_prepare' cb = global_prepare $ \g' -> acqG >>= \g -> cb (g', g)
    prepare' (g', g) cb = prepare g' $ \r' -> acqL g >>= \r -> cb (r', r)
    handle' (g', g) (r', r) i = handle g' r' (g,r,i)
    cleanup' (g', g) (r', r) = relL g r >> cleanup g' r'
    cleanup_global' (g', g) = relG g >> cleanup_global g'
