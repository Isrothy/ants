{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.ParagraphParams
  ( ParagraphParams (..),
  )
where

import Control.Lens
import Model.MarkdownAst.Classes.HasInline

data ParagraphParams il where
  ParagraphParams ::
    { _inline :: il
    } ->
    ParagraphParams il

makeLenses ''ParagraphParams

instance HasInline ParagraphParams il where
  inline = Model.MarkdownAst.Params.ParagraphParams.inline
