{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Streamly.Vinyl
    (
        -- * Transforming records
      groupBy
    , mapAheadlyValues

        -- * (Unboxed) Vector utilities
    , boundedToTrueHot
    , boundedToOneHot
    , maybeBoundedToOneHot
    )
where
import           Control.Lens                   ( ix
                                                , (&)
                                                , (.~)
                                                )

import           Data.Map.Monoidal              ( MonoidalMap )
import           Data.Map.Monoidal             as MMap
import qualified Data.Vector.Unboxed           as VU
import           Streamly                       ( MonadAsync
                                                , aheadly
                                                )
import qualified Streamly.Data.Fold            as FL
import qualified Streamly.Internal.Data.Fold   as FL
import qualified Streamly.Prelude              as S


{-# INLINE groupBy #-}

-- | Given a function for maybe extracting keys and a function
-- for extracting values, produce a `Fold` to a `MonoidalMap` of keys and values
--
groupBy
    :: (Monad m, Ord k, Monoid b)
    => (a -> Maybe k)
    -> (a -> b)
    -> FL.Fold m a (MonoidalMap k b)
groupBy kf vf = FL.Fold step' initial' extract'
  where
    initial' = return mempty
    step' kv a = return $ maybe kv (\k -> kv <> MMap.singleton k (vf a)) (kf a)
    extract' = return

{-# INLINE mapAheadlyValues #-}

-- | Streamly mapM the values in a MonoidalMap by converting the latter
-- to list, aheadly `Streamly.mapM`ing the function and collecting the result in a
-- `MonoidalMap`.  This is a quick, concurrent `fmap` over a `MonoidalMap`.
--
mapAheadlyValues
    :: (MonadAsync m, Eq k, Semigroup a)
    => (t -> m a)
    -> m (MonoidalMap k t)
    -> m (MonoidalMap k a)
mapAheadlyValues fn mp = do
    ph <- mp
    ol <-
        S.toList
        . S.mapM (secondM fn)
        . aheadly
        . S.fromList
        . MMap.toAscList
        $ ph
    pure $ MMap.fromAscListWith (<>) ol
  where
    secondM f (f', ms) = do
        s <- f ms
        pure (f', s)


-- | Given an `a` that is Bounded and Enum, we can create a "one-hot" repersentation
-- using `Bool`
--
boundedToTrueHot :: (Bounded a, Enum a) => a -> VU.Vector Bool
boundedToTrueHot (a :: a) = c
  where
    !c =
        VU.replicate (fromEnum (maxBound :: a) + 1) False
            &  ix (fromEnum a)
            .~ True

boundedToOneHot :: (Bounded a, Enum a) => a -> VU.Vector Int
boundedToOneHot (a :: a) = c
  where
    !c = VU.replicate (fromEnum (maxBound :: a) + 1) 0 & ix (fromEnum a) .~ 1

-- | Takes a default argument for missing data
--
maybeBoundedToOneHot :: (Bounded a, Enum a) => Int -> Maybe a -> VU.Vector Int
maybeBoundedToOneHot d (Nothing :: Maybe a) =
    VU.replicate (fromEnum (maxBound :: a) + 1) d
maybeBoundedToOneHot _ (Just a :: Maybe a) = c
  where
    !c = VU.replicate (fromEnum (maxBound :: a) + 1) 0 & ix (fromEnum a) .~ 1
