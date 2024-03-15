{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.SuperscriptParams
  ( SuperscriptParams (..),
  )
where

import Control.Lens
import Model.MarkdownAst.Classes.HasInline

data SuperscriptParams il where
  SuperscriptParams ::
    { _inline :: il
    } ->
    SuperscriptParams il

makeLenses ''SuperscriptParams

instance HasInline SuperscriptParams il where
  inline = Model.MarkdownAst.Params.SuperscriptParams.inline
