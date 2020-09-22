module Test.Main where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (message)
import GitHub.Actions.Core as Core
import GitHub.Actions.ToolCache as ToolCache

main :: Effect Unit
main = do
  Core.info "Starting test action..."
  result <- runExceptT do
    _ <- ToolCache.find' { toolName: "my-tool", versionSpec: "12.x" }
    ToolCache.find { toolName: "my-tool", versionSpec: "12.x", arch: Just "armv6" }
  case result of
    Left err -> Core.info (message err)
    Right _ -> Core.info "No errors in find"
  Core.info "Done running test action."
