{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.HeaderParams
  ( HeaderParams (..),
  )
where

import Control.Lens
import Model.MarkdownAst.Classes.HasInline
import Model.MarkdownAst.Classes.HasLevel

data HeaderParams il where
  HeaderParams ::
    { _level :: Int,
      _inline :: il
    } ->
    HeaderParams il

makeLenses ''HeaderParams

instance HasInline HeaderParams il where
  inline = Model.MarkdownAst.Params.HeaderParams.inline

instance HasLevel (HeaderParams il) where
  level = Model.MarkdownAst.Params.HeaderParams.level
