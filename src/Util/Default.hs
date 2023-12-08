{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Util.Default
  ( Default (..),
    unpackMaybe,
  )
where

import Data.Algebra.Boolean
import Data.Complex
import Data.Int
import Data.Maybe
import Data.Ratio
import qualified Data.Text as T
import Data.Word

class Default a where
  def :: a

instance Default Int where
  def = 0

instance Default Integer where
  def = 0

instance Default Int8 where
  def = 0

instance Default Int16 where
  def = 0

instance Default Int32 where
  def = 0

instance Default Int64 where
  def = 0

instance Default Word where
  def = 0

instance Default Word16 where
  def = 0

instance Default Word32 where
  def = 0

instance Default Word64 where
  def = 0

instance Default Float where
  def = 0

instance Default Double where
  def = 0

instance (RealFloat a) => Default (Complex a) where
  def = 0 :+ 0

instance (Integral a) => Default (Ratio a) where
  def = 0 % 1

instance Default Ordering where
  def = EQ

instance Default Char where
  def = '\0'

instance Default T.Text where
  def = T.pack ""

instance Default [a] where
  def = []

instance Default (Maybe a) where
  def = Nothing

instance Default Bool where
  def = False

instance (Boolean a) => Default a where
  def = false

instance Default (a -> a) where
  def = id

instance Default () where
  def = ()

instance (Default a, Default b) => Default (a, b) where
  def = (def, def)

instance (Default a, Default b, Default c) => Default (a, b, c) where
  def = (def, def, def)

instance (Default a, Default b, Default c, Default d) => Default (a, b, c, d) where
  def = (def, def, def, def)

instance (Default a, Default b, Default c, Default d, Default e) => Default (a, b, c, d, e) where
  def = (def, def, def, def, def)

instance (Default a, Default b, Default c, Default d, Default e, Default f) => Default (a, b, c, d, e, f) where
  def = (def, def, def, def, def, def)

instance (Default a, Default b, Default c, Default d, Default e, Default f, Default g) => Default (a, b, c, d, e, f, g) where
  def = (def, def, def, def, def, def, def)

unpackMaybe :: (Default a) => Maybe a -> a
unpackMaybe = fromMaybe def
