{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.StrikethroughParams
  ( StrikethroughParams (..),
  )
where

import Control.Lens ( makeLenses )
import Model.MarkdownAst.Classes.HasInline

data StrikethroughParams il where
  StrikethroughParams ::
    { _inline :: il
    } ->
    StrikethroughParams il

makeLenses ''StrikethroughParams

instance HasInline StrikethroughParams il where
  inline = Model.MarkdownAst.Params.StrikethroughParams.inline
