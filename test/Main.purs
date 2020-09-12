module Test.Main where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import GitHub.Actions.Core as Core

main :: Effect Unit
main = do
  Core.exportVariable { key: "test-key", value: "test-value" }
  Core.setSecret "test-key"
  Core.addPath "/path/to/test/tool"
  Core.setOutput { name: "test-output-key", value: "test-output-value" }
  void $ runExceptT do
    input <- Core.getInput' "test-input"
    liftEffect $ Core.info input
    Core.getInput { name: "test-input", options: Just { required: true } }
