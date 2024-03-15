{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.DisplayMathParams
  ( DisplayMathParams (..),
  )
where

import Control.Lens (makeLenses)
import Data.Text qualified as T
import Model.MarkdownAst.Classes.HasText

data DisplayMathParams where
  DisplayMathParams ::
    { _text :: T.Text
    } ->
    DisplayMathParams

makeLenses ''DisplayMathParams

instance HasText DisplayMathParams where
  text = Model.MarkdownAst.Params.DisplayMathParams.text
