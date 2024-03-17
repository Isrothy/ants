{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.EmojiParams
  ( EmojiParams (..),
  )
where

import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Lenses.HasAscii
import Model.MarkdownAst.Lenses.HasEmoji

data EmojiParams where
  EmojiParams ::
    { _ascii :: T.Text,
      _emoji :: T.Text
    } ->
    EmojiParams
  deriving (Show, Eq)

makeLenses ''EmojiParams

instance HasEmoji EmojiParams where
  emoji = Model.MarkdownAst.Params.EmojiParams.emoji

instance HasAscii EmojiParams where
  ascii = Model.MarkdownAst.Params.EmojiParams.ascii
