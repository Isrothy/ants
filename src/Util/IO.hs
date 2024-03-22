{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.IO
  ( readFileSafe,
    safeIO,
  )
where

import Control.Exception (IOException)
import Control.Exception.Base (try)
import Control.Monad
import Data.ByteString qualified
import Data.ByteString.Lazy qualified
import Data.Either
import Data.Function
import Data.Maybe
import Data.Text qualified
import Data.Text.IO qualified
import Data.Text.Lazy qualified
import Data.Text.Lazy.IO qualified
import GHC.IO
import Prelude qualified

class HasReadFile a where
  readFile :: Prelude.FilePath -> IO a

instance HasReadFile Data.Text.Text where
  readFile = Data.Text.IO.readFile

instance HasReadFile Prelude.String where
  readFile = Prelude.readFile

instance HasReadFile Data.Text.Lazy.Text where
  readFile = Data.Text.Lazy.IO.readFile

instance HasReadFile Data.ByteString.ByteString where
  readFile = Data.ByteString.readFile

instance HasReadFile Data.ByteString.Lazy.ByteString where
  readFile = Data.ByteString.Lazy.readFile

safeIO :: forall a. IO a -> IO (Maybe a)
safeIO io = do
  result <- try io :: IO (Either IOException a)
  return $ case result of
    Left _ -> Nothing
    Right content -> Just content

readFileSafe :: forall a. (HasReadFile a) => Prelude.FilePath -> IO (Maybe a)
readFileSafe = safeIO . readFile

-- result <- try (readFile filePath) :: IO (Either IOException a)
-- return $ case result of
--   Left _ -> Nothing
--   Right content -> Just content
