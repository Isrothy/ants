{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Cli.Graph
  ( printGraph,
  )
where

import Commonmark (ToPlainText (toPlainText))
import Control.Conditional (if')
import Control.Lens (view)
import Control.Monad.Extra (concatMapM, fromMaybeM)
import Control.Monad.Trans.Maybe
import Data.Graph.Inductive (Edge, Graph (mkGraph), Node, toLEdge)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz (GraphvizParams (..), graphToDot, toDot, GraphID(Str), GlobalAttributes (..), toLabel, NodeCluster (..), blankParams, setDirectedness, textLabel, DotGraph, quitWithoutGraphviz, runGraphviz, GraphvizOutput (..), graphvizWithHandle, GraphvizCommand (Dot), shape, Shape (..))
import Data.GraphViz.Printing (renderDot)
import Data.List (elemIndex, sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text.Lazy (unpack, pack)
import Model.Config (getSyntaxSpec)
import Model.Document (Document (..))
import Model.MarkdownAst (AstNode, MarkdownAst, findHeaders, findLinks, findWikiLinks, parameters, sourceRange)
import Model.MarkdownAst.Lenses (HasTarget (target), inline)
import Model.MarkdownAst.Params.HeaderParams
import Model.MarkdownAst.Params.LinkParams
import Model.MarkdownAst.Params.WikiLinkParams
import Parser.Markdown (MarkdownSyntax)
import Parser.Opts
import Path (Abs, Dir, Path, toFilePath)
import Path.IO (getCurrentDir, openTempFile, withTempFile)
import Project.DocLoader (loadAllFromDirectory)
import Project.Link (gotoLinkedElement)
import Project.ProjectRoot (findRoot, readConfig)
import Data.GraphViz.Attributes.Complete (Attribute(..))
import Data.ByteString (hGetContents)
import GHC.IO.Handle (hGetContents')

type GraphNode = (Document, Maybe (AstNode (HeaderParams MarkdownAst)))

getNodes :: Document -> ([Maybe (AstNode (HeaderParams MarkdownAst))], ([[AstNode (LinkParams MarkdownAst)]], [[AstNode (WikiLinkParams MarkdownAst)]]))
getNodes doc =
  fromMaybe
    ([Nothing], ([[]], [[]]))
    ( do
        as <- ast doc
        let sortedHeaders = sortOn (view sourceRange) $ findHeaders as
        let sortedLinks = sortOn (view sourceRange) $ findLinks as
        let sortedWikiLinks = sortOn (view sourceRange) $ findWikiLinks as
        let res = (comb (view sourceRange) (view sourceRange) sortedHeaders sortedLinks, comb (view sourceRange) (view sourceRange) sortedHeaders sortedWikiLinks)
              where
                comb f1 f2 (x : xs) (y : ys) =
                  let qwq = comb f1 f2 (x : xs) ys
                   in if' (f1 x < f2 y) ([] : comb f1 f2 xs (y : ys)) ((head qwq ++ [y]) : tail qwq)
                comb f1 f2 (_ : xs) [] = [] : comb f1 f2 xs []
                comb _ _ [] ys = [ys]

        return (Nothing : map Just sortedHeaders, res)
    )

getEdge :: (HasTarget (a MarkdownAst)) => MarkdownSyntax -> Path Abs Dir -> [GraphNode] -> Document -> AstNode (a MarkdownAst) -> IO (Maybe Node)
getEdge spec root nodes doc lk = runMaybeT $ do
  qwq <- MaybeT $ gotoLinkedElement spec root (absPath doc) (view (parameters . target) lk)
  MaybeT $ pure $ elemIndex qwq nodes

getEdges :: (HasTarget (a MarkdownAst)) => MarkdownSyntax -> Path Abs Dir -> [GraphNode] -> Document -> [AstNode (a MarkdownAst)] -> IO [Node]
getEdges spec root nodes doc lks = do
  ans <- mapM (getEdge spec root nodes doc) lks
  return $ catMaybes ans

getAllEdges :: MarkdownSyntax -> Path Abs Dir -> [GraphNode] -> (GraphNode, ([AstNode (LinkParams MarkdownAst)], [AstNode (WikiLinkParams MarkdownAst)])) -> IO [Edge]
getAllEdges spec root nodes docNode = do
  let curDoc = fst $ fst docNode
  linkEdges <- getEdges spec root nodes curDoc (fst $ snd docNode)
  wikiLinkEdges <- getEdges spec root nodes curDoc (snd $ snd docNode)
  let res = case elemIndex (fst docNode) nodes of
        Just x -> map (x,) (linkEdges ++ wikiLinkEdges)
        Nothing -> []
  return res

convert :: (a, ([b], ([[c]], [[d]]))) -> [((a, b), ([c], [d]))]
convert (x, (y, (z, t))) =
  map helper (zip3 y z t)
  where
    helper (u, zs, ts) = ((x, u), (zs, ts))

getGraph :: MarkdownSyntax -> Path Abs Dir -> [Document] -> IO (Gr GraphNode ())
getGraph spec root docs = do
  let docNodes = zip docs $ map getNodes docs
  let graphNodes = concatMap helper $ zip docs (map (fst . snd) docNodes)
        where
          helper (a, xs) = map (a,) xs
  edges <- concatMapM (getAllEdges spec root graphNodes) (concatMap convert docNodes)
  return $ mkGraph (zip [0 ..] graphNodes) (map (`toLEdge` ()) edges)
renderGr :: Bool -> DotGraph Node -> IO ()
renderGr False g = do
  let outputText = renderDot $ toDot g
  putStrLn $ unpack outputText
renderGr True g = do
  _ <- quitWithoutGraphviz "Graphviz should be installed"
  text <- graphvizWithHandle Dot g Svg hGetContents'
  putStrLn text
printGraph :: GraphOptions -> IO ()
printGraph opt = do
  pathToRoot <- fromMaybeM (error "Cannot find config") findRoot
  config <- fromMaybeM (error "config: Decode failed") $ readConfig pathToRoot
  cwd <- getCurrentDir
  docs <- loadAllFromDirectory (getSyntaxSpec config) cwd
  graph <- getGraph (getSyntaxSpec config) pathToRoot docs
  let graphInDotFormat =
        setDirectedness graphToDot params graph
        where
          params =
            blankParams
              { globalAttributes = [],
                clusterBy = clustBy,
                clusterID = Str,
                isDotCluster = const True,
                fmtCluster = clFmt,
                fmtNode = ndFmt,
                fmtEdge = const []
              }
          clustBy (n, l) = C (pack $ toFilePath $ relPath $ fst l) $ N (n, l)
          clFmt m = [GraphAttrs [toLabel m]]
          ndFmt (_, l) = case snd l of
            Just x -> [toLabel $ toPlainText $ view (parameters . inline) x]
            Nothing -> [shape PointShape]
  renderGr (isSVG opt) graphInDotFormat
