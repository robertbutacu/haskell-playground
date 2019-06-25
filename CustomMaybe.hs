module CustomMaybe where

import Data.Functor
import Control.Applicative
import Control.Monad

data CustomMaybe a = CustomNothing | CustomJust a deriving (Show, Eq)

instance Functor CustomMaybe where
    fmap f CustomNothing  = CustomNothing
    fmap f (CustomJust a) = CustomJust (f a)

instance Applicative CustomMaybe where
    pure a = CustomJust a
    (<*>) f CustomNothing  = CustomNothing
    (<*>) f (CustomJust a) = fmap (\ff -> ff a) f
