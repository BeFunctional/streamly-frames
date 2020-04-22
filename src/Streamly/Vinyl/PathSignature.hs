{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
module Streamly.Vinyl.PathSignature
    ( unsafeSignature
    , toVector
    , toBigVector
    , pathSignature
    , calcDimension
    , convolve
    , sign
    , getMAndD
    , PathSignature
  -- * Tests
    , unit_sig_correct
    , unit_serialize_roundtrip
    )
where
import           Data.Proxy                     ( Proxy(..) )
import "strict-tuple" Data.Tuple.Strict         ( T2(..) )

import           Codec.Serialise                ( Serialise
                                                , deserialise
                                                , serialise
                                                )
import           Codec.Serialise.Class          ( decode
                                                , encode
                                                )
import           Codec.Serialise.Decoding       ( decodeInt )
import           Codec.Serialise.Encoding       ( encodeInt )
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as VU
import           GHC.Generics                   ( Generic )
import           GHC.TypeLits                   ( KnownNat
                                                , Nat
                                                , natVal
                                                )
import           Streamly                       ( IsStream )
import qualified Streamly.Prelude              as S
-- TOOD: Make flat representaiton

data PathSignature (d :: Nat) (m :: Nat) a where
  Signature ::V.Vector (VU.Vector a) -> PathSignature d m a
  deriving stock (Eq, Ord, Generic, Show)

instance (Serialise a, VU.Unbox a, KnownNat d, KnownNat m) => Serialise (PathSignature d m a) where
    encode (Signature a) = encodeInt d <> encodeInt m <> encode a
      where
        m = fromIntegral $ natVal (Proxy :: Proxy m)
        d = fromIntegral $ natVal (Proxy :: Proxy d)
    decode = do
        d' <- decodeInt
        m' <- decodeInt
        if d' == d && m' == m
            then Signature <$> decode
            else
                error
                $  "expected: PathSignature "
                <> show d
                <> " "
                <> show m
                <> " got : PathSignature "
                <> show d'
                <> " "
                <> show m'
      where
        m = fromIntegral $ natVal (Proxy :: Proxy m)
        d = fromIntegral $ natVal (Proxy :: Proxy d)

-- | A Vector of componenets as Unboxed Vectors
--
toVector :: PathSignature d m a -> V.Vector (VU.Vector a)
toVector (Signature a) = a

-- | A single, flat Unboxed Vector
--
toBigVector :: VU.Unbox a => PathSignature d m a -> VU.Vector a
toBigVector (Signature a) = VU.concat (V.toList a)

instance (VU.Unbox a, Num a, KnownNat d, KnownNat m) => Semigroup (PathSignature d m a) where
    (<>) a b = convolve b a

instance (VU.Unbox a, Num a, KnownNat d, KnownNat m, Fractional a) => Monoid (PathSignature d m a) where
    mempty = unsafeSignature $ VU.replicate d 0
        where d = fromIntegral $ natVal (Proxy :: Proxy d)
    mappend = (<>)

sign
    :: forall m1 d a1 a2 t m
     . ( KnownNat m1
       , KnownNat d
       , VU.Unbox a1
       , Fractional a1
       , IsStream t
       , Monad m
       )
    => (a2 -> VU.Vector a1)
    -> t m a2
    -> t m (T2 a2 (PathSignature d m1 a1))
sign f = S.map fromJust' . S.drop 1 . S.postscanl' go Nothing
  where
    fromJust' Nothing =
        error
            "sign got a Nothing.  This will only happen if the term-level dimensions don't match the type-level specificaiton."
    fromJust' (Just v) = v
    checkD v' | VU.length v' == fromIntegral (natVal (Proxy :: Proxy d)) = v'
              | otherwise = error "sign: failed to get the expected dimensions!"
    go Nothing r = Just $ (T2 r $ (unsafeSignature . checkD) $ f r)
    go (Just (T2 _ v)) r =
        Just $ (T2 r $ (flip convolve v . unsafeSignature) $ f r)

-- | Calculate the signature of an Unboxed Vector
--
pathSignature
    :: forall d m a
     . (VU.Unbox a, Fractional a, KnownNat d, KnownNat m)
    => VU.Vector a
    -> Maybe (PathSignature d m a)
pathSignature v | checkD v  = Just $ unsafeSignature v
                | otherwise = Nothing
    where checkD v' = VU.length v' == fromIntegral (natVal (Proxy :: Proxy d))

unsafeSignature
    :: forall d m a
     . (KnownNat m, VU.Unbox a, Fractional a)
    => VU.Vector a
    -> PathSignature d m a
unsafeSignature v = Signature . V.fromList $ fmap (`calcKthComponent` v)
                                                  [0 .. m]
  where
    m = fromIntegral $ natVal (Proxy :: Proxy m)
    calcKthComponent k' v' =
        VU.map (/ fromIntegral (factorial k' :: Int)) $ caclKthprod k' v'
    caclKthprod k' v' | k' == 0   = VU.fromList [1]
                      | otherwise = kronecker v' (caclKthprod (k' - 1) v')

factorials :: (Integral a) => [a]
factorials = 1 : 1 : zipWith (*) [2 ..] (tail factorials)

factorial :: (Integral a) => Int -> a
factorial = (factorials !!)

kronecker :: (VU.Unbox a, Num a) => VU.Vector a -> VU.Vector a -> VU.Vector a
kronecker a = VU.concatMap (\a' -> VU.map (* a') a)

convolve
    :: forall d m a
     . (VU.Unbox a, Num a, KnownNat m)
    => PathSignature d m a
    -> PathSignature d m a
    -> PathSignature d m a
convolve (Signature a) (Signature b) =
    Signature . V.fromList $ calcK <$> [0 .. m]
  where
    calcIthOfK k i = kronecker (a V.! i) (b V.! (k - i))
    calcK k = foldl1 (VU.zipWith (+)) $ calcIthOfK k <$> [0 .. k]
    m = fromIntegral $ natVal (Proxy :: Proxy m)

calcDimension
    :: forall d m a . (KnownNat d, KnownNat m) => PathSignature d m a -> Int
calcDimension _ = calcDimension' m d
  where
    m = fromIntegral $ natVal (Proxy :: Proxy m) :: Int
    d = fromIntegral $ natVal (Proxy :: Proxy d)

calcDimension' :: (Integral b, Num a) => b -> a -> a
calcDimension' m d = sum $ (d ^) <$> [0 .. m]

getMAndD
    :: forall d m a
     . (KnownNat d, KnownNat m)
    => PathSignature d m a
    -> (Int, Integer)
getMAndD _ = (m, d)
  where
    m = fromIntegral $ natVal (Proxy :: Proxy m) :: Int
    d = natVal (Proxy :: Proxy d)

pathA :: PathSignature 2 2 Double
pathA = unsafeSignature v where v = VU.fromList [1, 1]

-- TODO: https://arxiv.org/pdf/1603.03788.pdf Computes statistical momements and cross correlation from path sigs

-- | Tests that the algebra results in the published caclulations in https://arxiv.org/pdf/1308.0371.pdf
-- Note the order of arguments in `convolve`.
--
unit_sig_correct :: Bool
unit_sig_correct =
    pathA == pathA' && pathB == pathB' && convolve pathB pathA == pathC'
  where

    pathB :: PathSignature 2 2 Double
    pathB = unsafeSignature v where v = VU.fromList [1, negate 1]

    pathA' :: PathSignature 2 2 Double
    pathA' = Signature
        (V.fromList
            [ VU.fromList [1]
            , VU.fromList [1, 1]
            , VU.fromList [0.5, 0.5, 0.5, 0.5]
            ]
        )
    pathB' = Signature
        (V.fromList
            [ VU.fromList [1]
            , VU.fromList [1, negate 1]
            , VU.fromList [0.5, negate 0.5, negate 0.5, 0.5]
            ]
        )
    pathC' = Signature
        (V.fromList
            [ VU.fromList [1]
            , VU.fromList [2, 0]
            , VU.fromList [2, negate 1, 1, 0]
            ]
        )

unit_serialize_roundtrip :: Bool
unit_serialize_roundtrip = pathA == roundTrip
    where roundTrip = deserialise $ serialise pathA
