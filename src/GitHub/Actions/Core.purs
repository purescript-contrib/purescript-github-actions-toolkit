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

import Control.Monad.Error.Class (try)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Promise (Promise, fromAff, toAffE)
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

-- | Interface for getInput options
type InputOptions =
  { required :: Boolean
  }

foreign import exportVariableImpl :: EffectFn2 String String Unit

-- | Sets env variable for this action and future actions in the job
exportVariable :: { key :: String, value :: String } -> Effect Unit
exportVariable { key, value } = runEffectFn2 exportVariableImpl key value

foreign import setSecretImpl :: EffectFn1 String Unit

-- | Registers a secret which will get masked from logs
setSecret :: String -> Effect Unit
setSecret = runEffectFn1 setSecretImpl

foreign import addPathImpl :: EffectFn1 String Unit

-- | Prepends input path to the PATH (for this action and future actions)
addPath :: String -> Effect Unit
addPath = runEffectFn1 addPathImpl

foreign import getInput1Impl :: EffectFn1 String String

foreign import getInput2Impl :: EffectFn2 String InputOptions String

-- | Gets the value of an input.  The value is also trimmed.
getInput
  :: { name :: String, options :: Maybe InputOptions }
  -> ExceptT Error Effect String
getInput { name, options } =
  maybe (runEffectFn1 getInput1Impl name) (runEffectFn2 getInput2Impl name) options
    # try >>> ExceptT

foreign import setOutputImpl :: EffectFn2 String String Unit

-- | Sets the value of an output.
setOutput :: { name :: String, value :: String } -> Effect Unit
setOutput { name, value } = runEffectFn2 setOutputImpl name value

foreign import setCommandEchoImpl :: EffectFn1 Boolean Unit

-- | Enables or disables the echoing of commands into stdout for the rest of the step.
-- | Echoing is disabled by default if ACTIONS_STEP_DEBUG is not set.
setCommandEcho :: Boolean -> Effect Unit
setCommandEcho = runEffectFn1 setCommandEchoImpl

foreign import setFailedImpl :: EffectFn1 String Unit

-- | Sets the action status to failed.
-- | When the action exits it will be with an exit code of 1
setFailed :: String -> Effect Unit
setFailed = runEffectFn1 setFailedImpl

foreign import isDebugImpl :: Effect Boolean

-- | Gets whether Actions Step Debug is on or not
isDebug :: Effect Boolean
isDebug = isDebugImpl

foreign import debugImpl :: String -> Effect Unit

-- | Writes debug message to user log
debug :: String -> Effect Unit
debug = debugImpl

foreign import errorImpl :: EffectFn1 String Unit

-- | Adds an error issue
error :: String -> Effect Unit
error = runEffectFn1 errorImpl

foreign import warningImpl :: EffectFn1 String Unit

-- | Adds a warning issue
warning :: String -> Effect Unit
warning = runEffectFn1 warningImpl

foreign import infoImpl :: EffectFn1 String Unit

-- | Writes info to log with console.log.
info :: String -> Effect Unit
info = runEffectFn1 infoImpl

foreign import startGroupImpl :: EffectFn1 String Unit

-- | Begin an output group.
-- | Output until the next `groupEnd` will be foldable in this group
startGroup :: String -> Effect Unit
startGroup = runEffectFn1 startGroupImpl

foreign import endGroupImpl :: EffectFn1 String Unit

-- | End an output group.
endGroup :: String -> Effect Unit
endGroup = runEffectFn1 endGroupImpl

foreign import saveStateImpl :: EffectFn2 String String Unit

-- | Saves state for current action.
-- | The state can only be retrieved by this action's post job execution.
saveState :: { name :: String, value :: String } -> Effect Unit
saveState { name, value } = runEffectFn2 saveStateImpl name value

foreign import getStateImpl :: EffectFn1 String String

-- | Gets the value of an state set by this action's main execution.
getState :: String -> Effect String
getState = runEffectFn1 getStateImpl

foreign import groupImpl :: forall a. EffectFn2 String (Effect (Promise a)) (Promise a)

-- | Wrap an asynchronous function call in a group.
group :: forall a. { name :: String, fn :: Aff a } -> Aff a
group { name, fn } = toAffE (runEffectFn2 groupImpl name (fromAff fn))
