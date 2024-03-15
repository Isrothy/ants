{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.EmphasisParams
  ( EmphasisParams (..),
  )
where

import Control.Lens (makeLenses)
import Model.MarkdownAst.Classes.HasInline

data EmphasisParams il where
  EmphasisParams ::
    { _inline :: il
    } ->
    EmphasisParams il

makeLenses ''EmphasisParams

instance HasInline EmphasisParams il where
  inline = Model.MarkdownAst.Params.EmphasisParams.inline
