module Data.Text.LineBreaker
  ( LineBreaker (..),
    splitLines,
    toString,
    joinLines,
  )
where

import Data.Maybe
import qualified Data.Text as T

data LineBreaker = LF | CRLF | CR
  deriving (Show, Eq)

toString :: LineBreaker -> String
toString LF = "\n"
toString CRLF = "\r\n"
toString CR = "\r"

splitLines :: T.Text -> [(T.Text, Maybe LineBreaker)]
splitLines txt = go txt Nothing
  where
    go t acc
      | T.null t = maybeToList acc
      | otherwise =
          let (line, rest) = T.break isLineBreak t
              isLineBreak c = c == '\n' || c == '\r'
              (newLine, newRest, lineBreak) = case T.uncons rest of
                Just ('\r', ns) -> case T.uncons ns of
                  Just ('\n', more) -> (line, more, Just CRLF)
                  _ -> (line, ns, Just CR)
                Just ('\n', ns) -> (line, ns, Just LF)
                _ -> (line, rest, Nothing)
           in maybeToList acc ++ go newRest (Just (newLine, lineBreak))

joinLines :: [(T.Text, Maybe LineBreaker)] -> T.Text
joinLines = T.concat . map (\(text, lineBreaker) -> text <> T.pack (maybe "" toString lineBreaker))
