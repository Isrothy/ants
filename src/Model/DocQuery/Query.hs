{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.DocQuery.Query
  ( Query (..),
    query,
    TaskType (..),
  )
where

import Commonmark hiding (plain)
import Commonmark.Extensions (AlertType)
import Control.Lens ((^.))
import Control.Monad.Extra
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Algebra.Boolean
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Model.DocQuery.BoolExpr
import Model.DocQuery.Term
import qualified Model.Document as D
import Model.MarkdownAst hiding (Alert)
import Model.MarkdownAst.Lenses
import Model.MarkdownAst.Params.AlertParams
import qualified Model.Metadata as M
import Path
import Project.Link
import Project.ProjectRoot
import Prelude hiding (and, any, not, or, (&&), (||))

type Filter a = a -> IO Bool

type DocFilter = Filter D.Document

type AstFilter = Filter (Maybe MarkdownAst)

type TextFilter = Filter T.Text

metadata :: (M.Metadata -> IO Bool) -> DocFilter
metadata f = f . D.metadata

ast :: AstFilter -> DocFilter
ast f = f . D.ast

ast' :: (MarkdownAst -> IO Bool) -> DocFilter
ast' = ast . maybe (return False)

relPath :: (Path Rel File -> IO Bool) -> DocFilter
relPath f = f . D.relPath

plain :: TextFilter -> AstFilter
plain f = maybe (return False) (f . toPlainText)

data TaskType = Done | Todo | Both
  deriving (Show, Eq)

class IsQuery a where
  query :: a -> DocFilter

data Query where
  Author :: (BoolExpr Term) -> Query
  Title :: (BoolExpr Term) -> Query
  Tag :: (BoolExpr Term) -> Query
  Description :: (BoolExpr Term) -> Query
  Content :: (BoolExpr Term) -> Query
  Task :: TaskType -> (BoolExpr Term) -> Query
  Alert :: AlertType -> (BoolExpr Term) -> Query
  DateTimeRange :: Maybe UTCTime -> Maybe UTCTime -> Query
  HasLink :: Path Rel File -> Query
  InDirectory :: Path Rel Dir -> Query
  deriving (Show, Eq)

instance IsQuery Query where
  query (Author t) = metadata $ \meta -> return $ match t $ fromMaybe "" $ M.author meta
  query (Title t) = metadata $ \meta -> return $ match t $ fromMaybe "" $ M.title meta
  query (Tag t) = metadata $ \meta -> return $ any (match t) (M.tags meta)
  query (Description t) = metadata $ \meta -> return $ match t $ M.description meta
  query (Content t) = (ast . plain) $ \content -> return $ match t content
  query (Task Both t) = ast' $ \md ->
    return $
      any (match t . toPlainText . snd) $
        findTasks md >>= (^. parameters . taskListItems)
  query (Task Done t) = ast' $ \md ->
    return $
      any (match t . toPlainText . snd) $
        filter fst $
          findTasks md >>= (^. parameters . taskListItems)
  query (Task Todo t) = ast' $ \md ->
    return $
      any (match t . toPlainText . snd) $
        filter (not . fst) $
          findTasks md >>= (^. parameters . taskListItems)
  query (Alert a t) = ast' $ \md ->
    return $
      any
        (\(AstNode (AlertParams at bl) _ _) -> (at == a) && (match t . toPlainText) bl)
        (findAlerts md)
  query (DateTimeRange start end) = metadata $ \meta ->
    return $ maybe False (between start end) (M.dateTime meta)
    where
      between (Just s) (Just e) d = s <= e && d >= s && d < e
      between (Just s) _ d = d >= s
      between _ (Just e) d = d < e
      between _ _ _ = True
  query (HasLink tar) = \doc -> do
    result <- runMaybeT $ do
      ast <- MaybeT $ return $ D.ast doc
      root <- MaybeT findRoot
      lift $
        anyM
          ( \filepath -> do
              t <- resolveLinkInFile (D.absPath doc) filepath
              return $ t == Just (root </> tar)
          )
          (map (T.unpack . (^. parameters . target)) (findLinks ast))
    return $ fromMaybe False result
  query (InDirectory dir) = relPath $ \path -> return $ dir `isProperPrefixOf` path

instance IsQuery (BoolExpr Query) where
  query (Val q) = query q
  query (And a b) = \doc -> do
    x <- query a doc
    y <- query b doc
    return $ x && y
  query (Or a b) = \doc -> do
    x <- query a doc
    y <- query b doc
    return $ x || y
  query (Not a) = \doc -> do
    x <- query a doc
    return $ not x
