{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.StrongParams
  ( StrongParams (..),
  )
where

import Control.Lens
import Model.MarkdownAst.Classes.HasInline

data StrongParams il where
  StrongParams ::
    { _inline :: il
    } ->
    StrongParams il

makeLenses ''StrongParams

instance HasInline StrongParams il where
  inline = Model.MarkdownAst.Params.StrongParams.inline
