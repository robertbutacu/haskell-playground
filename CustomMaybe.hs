module CustomMaybe where

import Data.Functor
import Control.Applicative
import Control.Monad

data CustomMaybe a = CustomNothing | CustomJust a

instance Functor CustomMaybe where
    fmap f CustomNothing  = CustomNothing
    fmap f (CustomJust a) = CustomJust (f a)
