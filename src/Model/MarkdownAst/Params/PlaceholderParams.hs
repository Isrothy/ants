{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.PlaceholderParams
  ( PlaceholderParams (..),
  )
where

import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Classes.HasText

data PlaceholderParams where
  PlaceholderParams ::
    { _text :: T.Text
    } ->
    PlaceholderParams

makeLenses ''PlaceholderParams

instance HasText PlaceholderParams where
  text = Model.MarkdownAst.Params.PlaceholderParams.text
