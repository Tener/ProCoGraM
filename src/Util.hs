module Util where

import System.Random.MWC.Monad
import Control.Monad.Primitive
import Control.Monad.Primitive.Class
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Time
import System.Locale

-- These functions were dropped from MWC Monad from version 0.2 to 0.3
-- added here
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import Debug.Trace

-- | Randomly select from list of equiprobable random sources. List must be non-empty
equiprobable :: (PrimMonad m, MonadPrim m) => [Rand m a] -> Rand m a
equiprobable [] = error "System.Random.MWC.Monad.equiprobable: list must be nonempty"
equiprobable xs = worker (V.fromList xs)
  where
    worker v = do
      -- uniform - 2^(-53) lies in the [0,1) range
      p <- uniform
      v V.! truncate ((p - 2^^(-53)) * fromIntegral (V.length v) :: Double)
      
-- | Randomly select from list of weighted random sources. List must
-- contain sources with positive weight. Elements with non-positive
-- weight are discarded
choices :: (PrimMonad m, MonadPrim m) => [(Double,Rand m a)] -> Rand m a
choices xs
  | null xs'  = error "System.Random.MWC.Monad.choices: list must contain at least one nonnegative weight"
  | otherwise = --aceShow (ps, V.length vect) 
                --
                worker vect ps
  where
    xs'  = filter ((>0) . fst) xs
    vect = V.fromList (map snd xs')
    ps   = U.init . U.scanl' (+) 0 $ U.map (/ U.sum q) q
           where q = U.fromList (map fst xs')
    worker vect probs = do
      p <- uniform
      let i = binary probs p
      vect V.! (i - 1)
      
-- Binary search /Copied from vector algorithms
binary :: (Ord a, U.Unbox a) => U.Vector a -> a -> Int
binary v x = binaryRange v x 0 (U.length v)
{-# INLINE binary #-}

-- Binary search in range
binaryRange :: (Ord a, U.Unbox a) => U.Vector a -> a -> Int -> Int -> Int
binaryRange v x = loop 
  where
    loop i j | i >= j    = j
             | otherwise = case compare (U.unsafeIndex v k) x of
                             LT -> loop (k+1) j
                             EQ -> k  
                             GT -> loop i k
             where k = (i + j) `div` 2
{-# INLINE binaryRange #-}

-- end of functions from mwc monad 0.2

-- Functions dropped from mwc monad 0.3 to 0.7


choices' :: (PrimMonad m, MonadPrim m) => [(Double, a)] -> Rand m a
choices' xs = choices [ (x, return y) | (x,y) <- xs]


equiprobable' :: (PrimMonad m, MonadPrim m) => [a] -> Rand m a
equiprobable' xs = equiprobable (map return xs)

modifyRandomElement :: (PrimMonad m, MonadPrim m) => [a] -> (a -> Rand m (Maybe a)) -> Rand m [a]
modifyRandomElement xs f = do
  let l = length xs
  i <- if l > 0 then uniformR (0,l-1) else return 0
  let (pre,(el:post)) = splitAt i xs
  el' <- f el
  case el' of
    Nothing -> return (pre ++ post)
    Just el'New -> return (pre ++ (el'New : post))

modifyRandomElementList :: (PrimMonad m, MonadPrim m) => [a] -> (a -> Rand m [a]) -> Rand m [a]
modifyRandomElementList xs f = do
  let l = length xs
  i <- if l > 0 then uniformR (0,l-1) else return 0
  let (pre,(el:post)) = splitAt i xs
  el' <- f el
  return (pre ++ el' ++ post)

modifyRandomKey :: (Ord k, PrimMonad m, MonadPrim m) => Map k v -> (v -> Rand m (Maybe v)) -> Rand m (Map k v)
modifyRandomKey mapa fun = do
  (k,v) <- equiprobable' (Map.assocs mapa)
  v' <- fun v
  return $ Map.update (const v') k mapa

modifyRandomKeyElement :: (Ord k, PrimMonad m, MonadPrim m) => Map k [a] -> (a -> Rand m (Maybe a)) -> Rand m (Map k [a])
modifyRandomKeyElement m f = modifyRandomKey m (\xs -> fmap Just $ modifyRandomElement xs f)

formatTimeExt now ext = formatTime defaultTimeLocale ("/tmp/foo_%s." ++ ext) now
