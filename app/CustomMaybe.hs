module CustomMaybe where

import Data.Functor
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Semigroup

data CustomMaybe a = CustomNothing | CustomJust a deriving (Show, Eq)

instance Functor CustomMaybe where
    fmap f CustomNothing  = CustomNothing
    fmap f (CustomJust a) = CustomJust (f a)

instance Applicative CustomMaybe where
    pure                   = CustomJust
    (<*>) f CustomNothing  = CustomNothing
    (<*>) f (CustomJust a) = fmap (\ff -> ff a) f

instance Monad CustomMaybe where
    (>>=) CustomNothing _  = CustomNothing
    (>>=) (CustomJust v) f = f v

instance Semigroup a => Semigroup (CustomMaybe a) where
    (<>) CustomNothing _ = CustomNothing
    (<>) _ CustomNothing = CustomNothing
    (<>) (CustomJust v1) (CustomJust v2) = CustomJust (v1 <> v2)

instance Semigroup a => Monoid (CustomMaybe a) where
    mempty = CustomNothing

someOtherF :: Semigroup a => CustomMaybe a -> CustomMaybe a -> CustomMaybe a
someOtherF cmA cmB = cmA <> cmB

someF :: CustomMaybe Int
someF = do x <- CustomJust 50
           y <- CustomJust 100
           z <- CustomNothing
           CustomJust (x + y)
