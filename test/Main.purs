module Test.Main where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import GitHub.Actions.ToolCache as ToolCache

main :: Effect Unit
main = do
  void $ runExceptT do
    _ <- ToolCache.find' { toolName: "my-tool", versionSpec: "12.x" }
    ToolCache.find { toolName: "my-tool", versionSpec: "12.x", arch: Just "armv6" }
