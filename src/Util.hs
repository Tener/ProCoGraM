module Util where

import System.Random.MWC.Monad
import Control.Monad.Primitive
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Time
import System.Locale

choices' :: PrimMonad m => [(Double, a)] -> Rand m a
choices' xs = choices [ (x, return y) | (x,y) <- xs]


equiprobable' :: PrimMonad m => [a] -> Rand m a
equiprobable' xs = equiprobable (map return xs)

modifyRandomElement :: PrimMonad m => [a] -> (a -> Rand m (Maybe a)) -> Rand m [a]
modifyRandomElement xs f = do
  let l = length xs
  i <- if l > 0 then uniformR (0,l-1) else return 0
  let (pre,(el:post)) = splitAt i xs
  el' <- f el
  case el' of
    Nothing -> return (pre ++ post)
    Just el'New -> return (pre ++ (el'New : post))

modifyRandomElementList :: PrimMonad m => [a] -> (a -> Rand m [a]) -> Rand m [a]
modifyRandomElementList xs f = do
  let l = length xs
  i <- if l > 0 then uniformR (0,l-1) else return 0
  let (pre,(el:post)) = splitAt i xs
  el' <- f el
  return (pre ++ el' ++ post)

modifyRandomKey :: (Ord k, PrimMonad m) => Map k v -> (v -> Rand m (Maybe v)) -> Rand m (Map k v)
modifyRandomKey mapa fun = do
  (k,v) <- equiprobable' (Map.assocs mapa)
  v' <- fun v
  return $ Map.update (const v') k mapa

modifyRandomKeyElement :: (Ord k, PrimMonad m) => Map k [a] -> (a -> Rand m (Maybe a)) -> Rand m (Map k [a])
modifyRandomKeyElement m f = modifyRandomKey m (\xs -> fmap Just $ modifyRandomElement xs f)

formatTimeExt now ext = formatTime defaultTimeLocale ("/tmp/foo_%s." ++ ext) now
