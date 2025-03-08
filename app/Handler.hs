{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Handler where 

import Prelude hiding (id, (.)) -- we get that from Control.Category

import Control.Concurrent.STM
import Control.Applicative

import Control.Category
import Control.Arrow

import Control.Lens

data Handler g r i o = Handler {
    global_prepare :: forall a. (g -> IO a) -> IO a
  , prepare :: forall a. (r -> IO a) -> IO a
  , handle :: g -> r -> i -> STM o
  , cleanup :: r -> IO ()
  , cleanup_global :: g -> IO ()
  }

eitherH :: Handler g r i o -> Handler g' r' i o -> Handler (g, g') (r, r') i o
eitherH = combineH (\a b i -> a i <|> b i)

composeH :: Handler g r i m -> Handler g' r' m o -> Handler (g, g') (r, r') i o
composeH = combineH (\a b i -> a i >>= b)

combineH :: ((i -> STM o) -> (i' -> STM o') -> (i'' -> STM o'')) -> Handler g r i o -> Handler g' r' i' o' -> Handler (g, g') (r, r') i'' o''
combineH w handler handler' = Handler global_prepare' prepare' handle' cleanup' cleanup_global' where
  global_prepare' c = global_prepare handler $ \g -> global_prepare handler' $ \g' -> c (g, g')
  prepare' c = prepare handler $ \p -> prepare handler' $ \p' -> c (p, p')
  handle' (g, g') (p, p') = w (handle handler g p) (handle handler' g' p')
  cleanup' (p, p') = cleanup handler p >> cleanup handler' p'
  cleanup_global' (g, g') = cleanup_global handler g >> cleanup_global handler' g'

mapH :: Iso' g g' -> Iso' r r' -> Iso (Handler g r i o) (Handler g r i o) (Handler g' r' i o) (Handler g' r' i o)
mapH ig ir = iso to' from' where
  to' (Handler {..}) = Handler global_prepare' prepare' handle' cleanup' cleanup_global' where
    global_prepare' cb = global_prepare $ cb . view ig
    prepare' cb = prepare $ cb . view ir
    handle' g' r' = handle (g' ^. from ig) (r' ^. from ir)
    cleanup' r' = cleanup $ r' ^. from ir
    cleanup_global' g' = cleanup_global $ g' ^. from ig
  from' (Handler {..}) = Handler global_prepare' prepare' handle' cleanup' cleanup_global' where
    global_prepare' cb = global_prepare $ cb . view (from ig)
    prepare' cb = prepare $ cb . view (from ir)
    handle' g r = handle (g ^. ig) (r ^. ir)
    cleanup' r = cleanup (r ^. ir)
    cleanup_global' g = cleanup_global (g ^. ig)

first' :: Iso (s,()) (t,()) s t
first' = iso (\(s,()) -> s) (\t -> (t,()))

liftSTM :: (i -> STM o) -> Handler () () i o
liftSTM f = Handler ($ ()) ($ ()) (const $ const $ f) return return 

instance Category (Handler () ()) where
  h . h' = view (mapH first' first') $ composeH h' h
  id = arr id
instance Arrow (Handler () ()) where
  h *** h' = view (mapH first' first') $ combineH (\a a' (i,i') -> (,) <$> a i <*> a' i') h h'
  arr f = liftSTM $ return . f

instance Profunctor (Handler g r) where
  dimap before after h = h { handle = handle' } where
    handle' g r i = fmap after $ handle h g r $ before i

withGlobal :: IO g -> (g -> IO ()) -> Handler g' r (g,i) o -> Handler (g',g) r i o
withGlobal acq rel h = view (mapH id first') $ withResources acq rel (return ()) return $ lmap (\(g,(),i) -> (g, i)) h

withResources :: IO g -> (g -> IO ()) -> IO r -> (r -> IO ()) -> Handler g' r' (g,r,i) o -> Handler (g',g) (r',r) i o
withResources acqG relG acqL relL (Handler {..}) = Handler global_prepare' prepare' handle' cleanup' cleanup_global' where
    global_prepare' cb = global_prepare $ \g' -> acqG >>= \g -> cb (g', g)
    prepare' cb = prepare $ \r' -> acqL >>= \r -> cb (r', r)
    handle' (g', g) (r', r) i = handle g' r' (g,r,i)
    cleanup' (r', r) = relL r >> cleanup r'
    cleanup_global' (g', g) = relG g >> cleanup_global g'
