{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.SpanParams
  ( SpanParams (..),
  )
where

data SpanParams x where
  SpanParams ::
    { ast :: [x]
    } ->
    SpanParams x
