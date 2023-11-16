module Common.Default
  ( Default (..),
  )
where

class Default a where
  def :: a

instance Default Int where
  def = 0

instance Default [a] where
  def = []

instance Default (Maybe a) where
  def = Nothing

instance Default Bool where
  def = False
