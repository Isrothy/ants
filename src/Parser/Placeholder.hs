{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Placeholder
  ( placeholderSpec,
    HasPlaceholder (..),
  )
where

import Commonmark
import Commonmark.Inlines
import Commonmark.TokParsers
import Data.Text hiding (concatMap)
import Text.Parsec

placeholderSpec ::
  (HasPlaceholder il, Monad m) => SyntaxSpec m il bl
placeholderSpec =
  mempty
    { syntaxInlineParsers =
        [parsePlaceholder]
    }

parsePlaceholder :: (Monad m, HasPlaceholder a) => InlineParser m a
parsePlaceholder = try $ do
  _ <- symbol '{'
  contents <- try $ untokenize <$> tockens
  return $ placeholder contents
  where
    tockens = do
      tk@(Tok toktype _ _) <- anyTok
      case toktype of
        Symbol '}' ->
          return []
        Symbol '\\' -> do
          tk' <- anyTok
          (tk :) . (tk' :) <$> tockens
        _ -> (tk :) <$> tockens

class HasPlaceholder a where
  placeholder :: Data.Text.Text -> a

instance HasPlaceholder (Html a) where
  placeholder t =
    addAttribute ("class", "placeholder") $
      htmlInline "span" $
        Just $
          htmlRaw "\\(" <> htmlText t <> htmlRaw "\\)"
