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
import qualified Data.Text as T
import qualified Model.Document as D
import Model.MarkdownAst
import Parser.MarkdownWithFrontmatter (MarkdownSyntax, markdownWithFrontmatter)
import Path
import Path.IO
import Project.DocLoader
import System.FilePath

isHeaderWithId :: T.Text -> MarkdownAstNode -> Bool
isHeaderWithId id (MarkdownAstNode (Header _ _) _ attrs) = lookup "id" attrs == Just id
isHeaderWithId _ _ = False

findHeaderWithId :: T.Text -> MarkdownAst -> Maybe MarkdownAstNode
findHeaderWithId id = firstNode (isHeaderWithId id)

type Bookmark = T.Text

isLink :: MarkdownAstNode -> Bool
isLink (MarkdownAstNode (Link {}) _ _) = True
isLink (MarkdownAstNode (WikiLink {}) _ _) = True
isLink _ = False

parseLink :: T.Text -> Maybe (T.Text, Maybe Bookmark)
parseLink input = case T.splitOn "#" input of
  [] -> Nothing
  [link] -> Just (link, Nothing)
  link : bookmark : _ -> Just (link, Just bookmark)

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
  IO (Maybe (D.Document, Maybe MarkdownAstNode))
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
