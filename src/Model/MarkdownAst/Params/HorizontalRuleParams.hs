{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.HorizontalRuleParams
  ( HorizontalRuleParams (..),
  )
where

import Control.Lens

data HorizontalRuleParams where
  HorizontalRuleParams ::
    {
    } ->
    HorizontalRuleParams
  deriving (Show, Eq)

makeLenses ''HorizontalRuleParams
