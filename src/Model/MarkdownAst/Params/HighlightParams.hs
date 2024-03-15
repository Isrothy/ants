{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.HighlightParams
  ( HighlightParams (..),
  )
where

import Control.Lens
import Model.MarkdownAst.Classes.HasInline

data HighlightParams il where
  HighlightParams ::
    { _inline :: il
    } ->
    HighlightParams il

makeLenses ''HighlightParams

instance HasInline HighlightParams il where
  inline = Model.MarkdownAst.Params.HighlightParams.inline
