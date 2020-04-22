{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE UnboxedTuples      #-}
module Streamly.Vinyl.LagLead
    ( LagLead(..)
    , lagLead
    )
where

import           GHC.Generics                   ( Generic )
import           Streamly                       ( IsStream )
import qualified Streamly.Prelude              as S


data LagLead a = LagLead { lead :: a, lag :: a}
    deriving stock (Eq, Ord, Show, Generic, Functor)

-- | A strict Either
--
data Se a b = R a | L b

isR :: Se a b -> Bool
isR (R _) = True
isR _     = False

fromR :: Se a b -> a
fromR (R v) = v
fromR _ =
    error "fromR:  This shouldn't have happend as this funciton is unexported!"


-- | A function that returns the input plus its one-lag
--
lagLead :: (IsStream t, Monad m) => t m a -> t m (LagLead a)
lagLead s =
    S.concatMap S.fromList . S.map (snd . fromR) . S.filter isR $ S.postscanl'
        go
        (L (LL Nothing, []))
        s
  where
    go (R (l@(LL (Just lg')), _)) a' =
        R (push l (Just a'), [LagLead a' lg', LagLead a' a'])
    go (L (l@(LL Nothing), _)) a = R (push l (Just a), [LagLead a a])
    go _                       _ = error "lagLead: Impossible!"
    push (LL _) = LL

newtype LL a = LL {_lg :: a}
    deriving Functor
