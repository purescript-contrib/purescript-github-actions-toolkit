-- | Exports functions from the @actions/core module provided by GitHub
-- | https://github.com/actions/toolkit/tree/main/packages/core
module GitHub.Actions.Core
  ( InputOptions
  , exportVariable
  , setSecret
  , addPath
  , getInput
  , setOutput
  , setCommandEcho
  , setFailed
  , isDebug
  , debug
  , error
  , warning
  , info
  , startGroup
  , endGroup
  , saveState
  , getState
  , group
  ) where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Promise (Promise, fromAff, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import GitHub.Actions.Utils (tryActionsM)

-- | Interface for getInput options
type InputOptions =
  { required :: Boolean
  }

foreign import exportVariableImpl :: { key :: String, value :: String } -> Effect Unit

-- | Sets env variable for this action and future actions in the job
exportVariable :: { key :: String, value :: String } -> Effect Unit
exportVariable = exportVariableImpl

foreign import setSecretImpl :: String -> Effect Unit

-- | Registers a secret which will get masked from logs
setSecret :: String -> Effect Unit
setSecret = setSecretImpl

foreign import addPathImpl :: String -> Effect Unit

-- | Prepends inputPath to the PATH (for this action and future actions)
addPath :: String -> Effect Unit
addPath = addPathImpl

foreign import getInputImpl
  :: { name :: String, options :: Nullable InputOptions }
  -> Effect (Nullable String)

-- | Gets the value of an input.  The value is also trimmed.
getInput
  :: { name :: String, options :: Maybe InputOptions }
  -> ExceptT Error Aff (Maybe String)
getInput { name, options } =
  getInputImpl { name, options: toNullable options }
    # liftEffect
    # tryActionsM
    # map toMaybe

foreign import setOutputImpl :: { name :: String, value :: String } -> Effect Unit

-- | Sets the value of an output.
setOutput :: { name :: String, value :: String } -> Effect Unit
setOutput = setOutputImpl

foreign import setCommandEchoImpl :: Boolean -> Effect Unit

-- | Enables or disables the echoing of commands into stdout for the rest of the step.
-- | Echoing is disabled by default if ACTIONS_STEP_DEBUG is not set.
setCommandEcho :: Boolean -> Effect Unit
setCommandEcho = setCommandEchoImpl

foreign import setFailedImpl :: String -> Effect Unit

-- | Sets the action status to failed.
-- | When the action exits it will be with an exit code of 1
setFailed :: String -> Effect Unit
setFailed = setFailedImpl

foreign import isDebugImpl :: Effect Boolean

-- | Gets whether Actions Step Debug is on or not
isDebug :: Effect Boolean
isDebug = isDebugImpl

foreign import debugImpl :: String -> Effect Unit

-- | Writes debug message to user log
debug :: String -> Effect Unit
debug = debugImpl

foreign import errorImpl :: String -> Effect Unit

-- | Adds an error issue
error :: String -> Effect Unit
error = errorImpl

foreign import warningImpl :: String -> Effect Unit

-- | Adds a warning issue
warning :: String -> Effect Unit
warning = warningImpl

foreign import infoImpl :: String -> Effect Unit

-- | Writes info to log with console.log.
info :: String -> Effect Unit
info = infoImpl

foreign import startGroupImpl :: String -> Effect Unit

-- | Begin an output group.
-- | Output until the next `groupEnd` will be foldable in this group
startGroup :: String -> Effect Unit
startGroup = startGroupImpl

foreign import endGroupImpl :: String -> Effect Unit

-- | End an output group.
endGroup :: String -> Effect Unit
endGroup = endGroupImpl

foreign import saveStateImpl :: { name :: String, value :: String } -> Effect Unit

-- | Saves state for current action.
-- | The state can only be retrieved by this action's post job execution.
saveState :: { name :: String, value :: String } -> Effect Unit
saveState = saveStateImpl

foreign import getStateImpl :: String -> Effect (Nullable String)

-- | Gets the value of an state set by this action's main execution.
getState :: String -> Effect (Maybe String)
getState = getStateImpl >>> map toMaybe

foreign import groupImpl
  :: forall a
   . { name :: String, fn :: Effect (Promise a) }
  -> Effect (Promise a)

-- | Wrap an asynchronous function call in a group.
group :: forall a. { name :: String, fn :: Aff a } -> Aff a
group { name, fn } =
  toAffE (groupImpl { name, fn: fromAff fn })
