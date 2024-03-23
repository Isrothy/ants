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
    parseAbsOrRelFile,
    findBookmarkLine,
    parseLink,
    findHeaderWithId,
    hasLinkTo,
    linksTo,
  )
where

import Commonmark
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.List (find)
import Data.Maybe
import qualified Data.Text as T
import qualified Model.Document as D
import Model.MarkdownAst
import Model.MarkdownAst.Lenses
import Model.MarkdownAst.Params.HeaderParams
import Parser.MarkdownWithFrontmatter (MarkdownSyntax)
import Path
import Path.IO
import Project.DocLoader
import System.FilePath hiding ((</>))

type Bookmark = T.Text

findHeaderWithId :: T.Text -> MarkdownAst -> Maybe (AstNode (HeaderParams MarkdownAst))
findHeaderWithId id ast = find (\node -> lookup "id" (node ^. attributes) == Just id) (findHaders ast)

isLink :: MdNode -> Bool
isLink (AstNode (Link {}) _ _) = True
isLink (AstNode (WikiLink {}) _ _) = True
isLink _ = False

parseLink :: T.Text -> Maybe (FilePath, Maybe Bookmark)
parseLink input = case T.splitOn "#" input of
  [link] -> Just (T.unpack link, Nothing)
  [link, bookmark] -> Just (T.unpack link, Just bookmark)
  _ -> Nothing

parseAbsOrRelFile :: Path Abs Dir -> FilePath -> Maybe (Path Abs File)
parseAbsOrRelFile root file = parseAbsFile file <|> fmap (root </>) (parseRelFile file)

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
  IO (Maybe (D.Document, Maybe (AstNode (HeaderParams MarkdownAst))))
gotoLinkedElement spec root orig txt =
  runMaybeT $ do
    (link, tag) <- MaybeT $ return $ parseLink txt
    path <- MaybeT $ resolveLinkInFile orig link
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

hasLinkTo :: Path Abs Dir -> Path Abs File -> MarkdownAst -> (FilePath, Maybe Bookmark) -> IO Bool
hasLinkTo root orig ast target = linksTo root orig ast target <&> (not . null)

linksTo :: Path Abs Dir -> Path Abs File -> MarkdownAst -> (FilePath, Maybe Bookmark) -> IO [SourceRange]
linksTo root orig ast (targetFilePath, targetTag) = do
  result <- runMaybeT $ do
    targetAbsPath <- MaybeT $ return $ parseAbsOrRelFile root targetFilePath
    let links = map (\x -> (x ^. (parameters . target), x ^. sourceRange)) (findLinks ast)
    let wikiLinks = map (\x -> (x ^. (parameters . target), x ^. sourceRange)) (findWikiLinks ast)
    lift $
      concatMapM
        ( \(tar, sr) -> case parseLink tar of
            Nothing -> return []
            Just (thisFilePath, thisTag) -> do
              t <- resolveLinkInFile orig thisFilePath
              if t == Just targetAbsPath && (isNothing targetTag || thisTag == targetTag)
                then return $ maybeToList sr
                else return []
        )
        (links ++ wikiLinks)
  return $ fromMaybe [] result
