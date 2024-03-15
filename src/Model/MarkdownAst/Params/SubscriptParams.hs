{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.SubscriptParams
  ( SubscriptParams (..),
  )
where

import Control.Lens (makeLenses)
import Model.MarkdownAst.Classes.HasInline

data SubscriptParams il where
  SubscriptParams ::
    { _inline :: il
    } ->
    SubscriptParams il

makeLenses ''SubscriptParams

instance HasInline SubscriptParams il where
  inline = Model.MarkdownAst.Params.SubscriptParams.inline
