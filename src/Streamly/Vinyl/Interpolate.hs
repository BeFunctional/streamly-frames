{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

-- | This module defines interpolation functions for dealing with streams of value. 
-- The implementations are decidedly inelegant and a bit convoluted.  When streamly exposes 
-- the parsers and terminating folds combinators this should be revisited. 
--
module Streamly.Vinyl.Interpolate
    ( interpolateLinear
    , interpolateWith
    , InterpolationFunction
    , propLengthsTheSame
    , propOrigValesTheSame
    )
where
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Trans.Control    ( MonadBaseControl )
import           Data.Either.Strict
import           Data.Maybe                     ( fromMaybe )
import "strict-tuple" Data.Tuple.Strict         ( T2(..) )
import           Prelude                 hiding ( Either(..) )
import           Streamly                       ( IsStream )
import qualified Streamly.Prelude              as S



type StateStep a b v = (T2 (Maybe (a, b)) [Int -> a -> v])
type StateResult a b v = (T2 (Maybe (a, b)) [v])

type State a b v = Either (StateStep a b v) (StateResult a b v)

-- | Interpolate missing values in a stream linearly
--
-- >>> import Data.Maybe ( fromMaybe, catMaybes )
-- >>> let interpolateMe = [Nothing, Nothing, Just 3, Nothing, Nothing, Just 9, Nothing, Nothing]
-- >>> fmap catMaybes . S.toList $ interpolateLinear (fromMaybe 0) id (\_ v -> Just v) (S.fromList interpolateMe)
-- [0.0,1.5,3.0,5.0,7.0,9.0,9.0,9.0]
--
interpolateLinear
    :: ( IsStream s
       , Fractional a
       , MonadIO m
       , MonadBaseControl IO m
       , MonadThrow m
       )
    => (Maybe a -> a) -- ^ a function providing a default value for any `Nothing` values existing before the first `Just`
    -> (v -> Maybe a) -- ^ a function that gets the value to be interpolated
    -> (v -> a -> v) -- ^ a function that puts the interpolated value
    -> s m v -- ^ The stream overwhich to interpolate
    -> s m v
interpolateLinear = interpolateWith linearInterpolation

-- | Interpolate some missing values given an `InterpolationFunction a`, a function providing values for
-- the first missing value preceeding the first value (if it exists), a function that "gets" the value to be obtained
-- a function that "puts" the value.
--
interpolateWith
    :: ( IsStream s
       , Fractional a
       , MonadIO m
       , MonadBaseControl IO m
       , MonadThrow m
       )
    => InterpolationFunction a -- ^ an interpolation function
    -> (Maybe a -> a) -- ^ a function providing a default value for any `Nothing` values existing before the first `Just`
    -> (v -> Maybe a) -- ^ a function that gets the value to be interpolated
    -> (v -> a -> v) -- ^ a function that puts the interpolated value
    -> s m v -- ^ The stream overwhich to interpolate
    -> s m v
interpolateWith interpFn defaultFn getFn putFn =
    S.concatMap S.fromList
        . S.map juggle
        . bufferRight -- need to either take right or take last left if the stream ends.
        . S.postscanl'
              (\acc v -> interpolate' interpFn defaultFn (putFn v) acc (getFn v)
              )
              init'
  where
    juggle (Right (T2 _ a)) = reverse a
    juggle _                = error "interpolate: juggle called on left!"

bufferRight
    :: (IsStream t, Monad m, Semigroup (t m (Maybe (State a Int v))))
    => t m (State a Int v)
    -> t m (State a Int v)
bufferRight s = S.filter isRight
    $ S.postscanl' buffer init' (S.map Just s <> S.fromList [Nothing])

buffer :: State a Int v -> Maybe (State a Int v) -> State a Int v
buffer _ (Just r') = r'
buffer (Left (T2 (Just (sv, sp)) xs)) Nothing =
    Right (T2 Nothing (capLerps sp sv xs))
buffer (Right (T2 _ _)) Nothing = init' -- end o the line.
buffer _                _       = error "buffer: impossible!"

init' :: State a Int v
init' = Left (T2 Nothing [])

-- | An interpolation function
type InterpolationFunction a = a -- ^ The starting value in the interval to be interpolated
    -> Int -- ^ The integral position of the current (interpolated) result
    -> Int -- ^ The total number of positions over which to interpolate
    -> a -- ^ The last value
    -> a -- ^ Interpolated value at the current position

linearInterpolation :: (Fractional a) => InterpolationFunction a
linearInterpolation startval currentPos totalpos endVal =
    ((endVal - startval) * (fromIntegral currentPos * (1.0 / fromIntegral totalpos)))
        + startval

capLerps :: Functor f => t1 -> t2 -> f (t1 -> t2 -> b) -> f b
capLerps tp e = fmap (\f' -> f' tp e)


-- TODO: No one wants to understand or maintain this.  When `streamly` has proper scanning
-- primitives, maybe we can make this better.
--
interpolate'
    :: (Fractional a)
    => InterpolationFunction a
    -> (Maybe a -> a) -- ^ Default function
    -> (a -> v)   -- ^ a curried put function
    -> State a Int v
    -> Maybe a
    -> State a Int v
interpolate' _ f put (Left (T2 Nothing _)) Nothing =
    Right (T2 (Just (val,0)) [put val]) -- recieved no values yet
    where 
        val = f Nothing 
interpolate' interpFn f put (Right _) r@(Just _) =
    interpolate' interpFn f put init' r -- if we get a right, start over. note: must only issue right when the next item is just
interpolate' _ f put (Right (T2 Nothing _)) Nothing =
    Right (T2 (Just (val,0)) [put val]) -- recieved no values yet
    where 
        val = f Nothing 
interpolate' interpFn _ put (Right (T2 (Just (sv, sp)) _)) Nothing =
    Left (T2 (Just (sv, sp + 1)) [(\i a -> put $ interpFn sv (sp + 1) i a)])
interpolate' _ _ put (Left (T2 Nothing _)) (Just r) =
    Right (T2 (Just (r, 0)) [put r]) -- first value received
interpolate' _ _ put (Left (T2 (Just _) [])) (Just r) =
    Right (T2 (Just (r, 0)) [put r]) -- second or later value received
interpolate' interpFn _ put (Left (T2 (Just (sv, sp)) xs)) Nothing = Left
    (T2 (Just (sv, sp + 1)) ((\i a -> put $ interpFn sv (sp + 1) i a) : xs)) -- second or later value received
interpolate' _ _ put (Left (T2 (Just (_sv, sp)) xs)) (Just r) =
    Right (T2 (Just (r, 0)) (put r : capLerps (sp + 1) r xs)) -- convert continuations



propLengthsTheSame :: [Maybe Double] -> IO Bool
propLengthsTheSame [] = pure True
propLengthsTheSame xs = do
    sxs'    <- S.toList sxs
    interp' <- S.toList interp
    pure (length sxs' == length interp')
  where
    sxs :: (IsStream t, Monad m) => t m (Maybe Double)
    sxs = S.fromList xs
    interp
        :: (IsStream t, MonadIO m, MonadBaseControl IO m, MonadThrow m)
        => t m (Maybe Double)
    interp = interpolateLinear (fromMaybe 0.0) id const sxs

propOrigValesTheSame :: [Maybe Double] -> IO Bool
propOrigValesTheSame [] = pure True
propOrigValesTheSame xs = do
    sxs'    <- S.toList sxs
    interp' <- S.toList interp
    pure $ and (zipWith check sxs' interp')
  where
    check o i = case (o, i) of
        (Just ov, Just iv) -> ov == iv
        (Nothing, Nothing) -> True -- If the list is only Nothings, this can happen.
        (_      , Nothing) -> False
        (Nothing, _      ) -> True
    sxs :: (IsStream t, Monad m) => t m (Maybe Double)
    sxs = S.fromList xs
    interp
        :: (IsStream t, MonadIO m, MonadBaseControl IO m, MonadThrow m)
        => t m (Maybe Double)
    interp = interpolateLinear (fromMaybe 0.0) id const sxs
