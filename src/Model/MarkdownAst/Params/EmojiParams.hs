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
import Model.MarkdownAst.Classes.HasText

data EmojiParams where
  EmojiParams ::
    { _text :: T.Text
    } ->
    EmojiParams

makeLenses ''EmojiParams

instance HasText EmojiParams where
  text = Model.MarkdownAst.Params.EmojiParams.text
