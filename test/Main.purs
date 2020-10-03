module Test.Main where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import GitHub.Actions.Core as Core
import GitHub.Actions.ToolCache as ToolCache

main :: Effect Unit
main = do
  Core.info "Starting test action..."

  -- Tests for GitHub.Actions.Core
  Core.exportVariable { key: "testkey", value: "test" }
  Core.setOutput { name: "testoutput", value: "test" }
  Core.setSecret "testoutput"
  Core.addPath "/test/path"
  resultCore <- runExceptT do
    _ <- Core.getInput' "testinput"
    _ <- Core.getInput { name: "testinput", options: Nothing }
    Core.getInput { name: "testinput", options: Just { required: true } }
  case resultCore of
    Left err -> Core.setFailed (message err)
    Right _ -> Core.info "No errors in GitHub.Actions.Core ExceptT functions"
  Core.setCommandEcho false
  _ <- Core.isDebug
  Core.debug "Testing debug"
  Core.error "Testing error"
  Core.warning "Testing warning"
  Core.info "Testing info"
  Core.startGroup "testgroup"
  Core.endGroup
  Core.saveState { name: "teststate", value: "test" }
  _ <- Core.getState "teststate"
  launchAff_ $ Core.group
    { name: "testGroup"
    , fn: liftEffect (pure unit)
    }

  -- Tests for GitHub.Actions.ToolCache
  result <- runExceptT do
    _ <- ToolCache.find' { toolName: "my-tool", versionSpec: "12.x" }
    ToolCache.find { toolName: "my-tool", versionSpec: "12.x", arch: Just "armv6" }
  case result of
    Left err -> Core.setFailed (message err)
    Right _ -> Core.info "No errors in find"
  Core.info "Done running test action."
