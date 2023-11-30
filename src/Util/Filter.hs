module Util.Filter
  ( Filter (..),
    and,
    or,
    Util.Filter.not,
    xor,
    true,
    false,
  )
where

import Prelude hiding (and, or)

data Filter a = Filter
  { filt :: a -> Bool
  }

and :: (Filter a) -> (Filter a) -> (Filter a)
and f1 f2 = Filter $ \x -> filt f1 x && filt f2 x

or :: (Filter a) -> (Filter a) -> (Filter a)
or f1 f2 = Filter $ \x -> filt f1 x || filt f2 x

not :: (Filter a) -> (Filter a)
not f = Filter $ \x -> Prelude.not $ filt f x

xor :: (Filter a) -> (Filter a) -> (Filter a)
xor f1 f2 = Filter $ \x -> case (filt f1 x, filt f2 x) of
  (True, True) -> False
  (False, False) -> False
  _ -> True

true :: Filter a
true = Filter $ const True

false :: Filter a
false = Filter $ const False

instance Semigroup (Filter a) where
  (<>) = and

instance Monoid (Filter a) where
  mempty = true
