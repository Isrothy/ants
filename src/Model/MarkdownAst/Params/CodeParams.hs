{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.CodeParams
  ( CodeParams (..),
  )
where

import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Lenses.HasText

data CodeParams where
  CodeParams ::
    { _text :: T.Text
    } ->
    CodeParams
  deriving (Show, Eq)

makeLenses ''CodeParams

instance HasText CodeParams where
  text = Model.MarkdownAst.Params.CodeParams.text
