{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Project.Link where

import Control.Exception
import qualified Data.Text as T
import Model.MarkdownAst
import Path
import Path.IO

headerWithId :: MarkdownAstNode -> T.Text -> Bool
headerWithId (MarkdownAstNode (Header _ _) _ attrs) id = lookup "id" attrs == Just id
headerWithId _ _ = False

parseLink :: T.Text -> Maybe (T.Text, Maybe T.Text)
parseLink input = case T.splitOn "#" input of
  [] -> Nothing
  [link] -> Just (link, Nothing)
  link : bookmark : _ -> Just (link, Just bookmark)

followLink :: Path Abs File -> FilePath -> IO (Path Abs File)
followLink orig link = do
  let file = parseAbsFile link
  case file of
    Just p -> return p
    Nothing -> resolveFile (parent orig) link
