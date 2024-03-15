{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.DoubleQuotedParams
  ( DoubleQuotedParams (..),
  )
where

import Control.Lens ( makeLenses )
import Model.MarkdownAst.Classes.HasInline ( HasInline(..) )

data DoubleQuotedParams il where
  DoubleQuotedParams ::
    { _inline :: il
    } ->
    DoubleQuotedParams il

makeLenses ''DoubleQuotedParams

instance HasInline DoubleQuotedParams il where
  inline = Model.MarkdownAst.Params.DoubleQuotedParams.inline
