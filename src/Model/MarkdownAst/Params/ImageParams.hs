{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.ImageParams
  ( ImageParams (..),
  )
where

import Control.Lens (makeLenses)
import Data.Text qualified as T
import Model.MarkdownAst.Classes.HasInline
import Model.MarkdownAst.Classes.HasTarget
import Model.MarkdownAst.Classes.HasTitle

data ImageParams il where
  ImageParams ::
    { _target :: T.Text,
      _title :: T.Text,
      _inline :: il
    } ->
    ImageParams il
  deriving (Show, Eq)

makeLenses ''ImageParams

instance HasInline ImageParams il where
  inline = Model.MarkdownAst.Params.ImageParams.inline

instance HasTarget (ImageParams il) where
  target = Model.MarkdownAst.Params.ImageParams.target

instance HasTitle (ImageParams il) where
  title = Model.MarkdownAst.Params.ImageParams.title
