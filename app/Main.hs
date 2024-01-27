module Main (main) where

import Lib (entrance)
import Options.Applicative
import Parser.Opts (opts)

main :: IO ()
main = entrance =<< execParser optsInfo
  where
    optsInfo =
      info
        (opts <**> helper)
        ( fullDesc
            <> progDesc "A command line interface for ants"
            <> header "ants - A Note Taking System"
        )
