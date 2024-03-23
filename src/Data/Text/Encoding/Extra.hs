module Data.Text.Encoding.Extra where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

toUTF16Offset :: T.Text -> Int -> Maybe Int
toUTF16Offset text offset
  | offset <= T.length text = Just $ B.length $ TE.encodeUtf16LE (T.take offset text)
  | otherwise = Nothing
