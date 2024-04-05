{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lsp.Handlers.Initialized where

import Control.Lens ((.=))
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Maybe
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Lsp.State
import Lsp.Util
import Model.Config
import Path
import Project.ProjectRoot (readConfig)

initializedHandler :: LSP.Handlers HandlerM
initializedHandler =
  LSP.notificationHandler LSP.SMethod_Initialized \_ -> do
    mconfig <- runMaybeT $ do
      mroot <- MaybeT $ liftLSP LSP.getRootPath
      root <- MaybeT $ return $ parseAbsDir mroot
      MaybeT $ liftIO $ readConfig root
    markdownSyntaxSpec .= getSyntaxSpec (fromMaybe def mconfig)
    return ()
