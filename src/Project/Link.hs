{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Project.Link
  ( resolveLinkInFile,
    gotoLinkedElement,
    isLink,
    Bookmark,
    findBookmarkLine,
    parseLink,
    findHeaderWithId,
  )
where

import Commonmark
import Control.Conditional
import Control.Lens
import Control.Monad (mzero)
import Control.Monad.Trans.Maybe
import Data.List (find)
import qualified Data.Text as T
import qualified Model.Document as D
import Model.MarkdownAst
import Model.MarkdownAst.Params.HeaderParams
import Parser.MarkdownWithFrontmatter (MarkdownSyntax)
import Path
import Path.IO
import Project.DocLoader
import System.FilePath

type Bookmark = T.Text

findHeaderWithId :: T.Text -> MarkdownAst -> Maybe (AstNode (HeaderParams InlineAst))
findHeaderWithId id ast = find (\node -> lookup "id" (node ^. attributes) == Just id) (findHaders ast)

isLink :: MdNode k -> Bool
isLink (AstNode (Link {}) _ _) = True
isLink (AstNode (WikiLink {}) _ _) = True
isLink _ = False

parseLink :: T.Text -> Maybe (T.Text, Maybe Bookmark)
parseLink input = case T.splitOn "#" input of
  [] -> Nothing
  [link] -> Just (link, Nothing)
  [link, bookmark] -> Just (link, Just bookmark)
  _ -> Nothing

resolveLinkInFile :: Path Abs File -> FilePath -> IO (Maybe (Path Abs File))
resolveLinkInFile orig link
  | null link = return $ Just orig
  | not (isValid link) = return Nothing
  | isAbsolute link = return $ parseAbsFile link
  | otherwise = Just <$> resolveFile (parent orig) link

gotoLinkedElement ::
  MarkdownSyntax ->
  Path Abs Dir ->
  Path Abs File ->
  T.Text ->
  IO (Maybe (D.Document, Maybe (AstNode (HeaderParams InlineAst))))
gotoLinkedElement spec root orig txt =
  runMaybeT $ do
    (link, tag) <- MaybeT $ return $ parseLink txt
    path <- MaybeT $ resolveLinkInFile orig (T.unpack link)
    exist <- doesFileExist path
    unless exist mzero
    rel <- MaybeT $ return $ stripProperPrefix root path
    doc <- MaybeT $ Just <$> loadDocument spec root rel
    ast <- MaybeT $ loadDocument spec root rel <&> D.ast
    return (doc, tag >>= (`findHeaderWithId` ast))

findBookmarkLine :: Bookmark -> MarkdownAst -> Maybe Int
findBookmarkLine bookmark ast = do
  ele <- findHeaderWithId bookmark ast
  sr <- ele ^. sourceRange
  case unSourceRange sr of
    (begin, _) : _ -> Just $ sourceLine begin
    _ -> Nothing
