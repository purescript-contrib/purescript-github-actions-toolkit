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

import Control.Promise (Promise, fromAff, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)

-- | Interface for getInput options
type InputOptions =
  { required :: Boolean
  }

-- | Sets env variable for this action and future actions in the job
foreign import exportVariable :: { key :: String, value :: String } -> Effect Unit

-- | Registers a secret which will get masked from logs
foreign import setSecret :: String -> Effect Unit

-- | Prepends inputPath to the PATH (for this action and future actions)
foreign import addPath :: String -> Effect Unit

foreign import getInputImpl
  :: { name :: String, options :: Nullable InputOptions }
  -> Effect (Nullable String)

-- TODO: handle thrown error
-- | Gets the value of an input.  The value is also trimmed.
getInput
  :: { name :: String, options :: Maybe InputOptions }
  -> Effect (Maybe String)
getInput { name, options } =
  map toMaybe (getInputImpl { name, options: toNullable options })

-- | Sets the value of an output.
foreign import setOutput :: { name :: String, value :: String } -> Effect Unit

-- | Enables or disables the echoing of commands into stdout for the rest of the step.
-- | Echoing is disabled by default if ACTIONS_STEP_DEBUG is not set.
foreign import setCommandEcho :: Boolean -> Effect Unit

-- | Sets the action status to failed.
-- | When the action exits it will be with an exit code of 1
foreign import setFailed :: String -> Effect Unit

-- | Gets whether Actions Step Debug is on or not
foreign import isDebug :: Effect Boolean

-- | Writes debug message to user log
foreign import debug :: String -> Effect Unit

-- | Adds an error issue
foreign import error :: String -> Effect Unit

-- | Adds an warning issue
foreign import warning :: String -> Effect Unit

-- | Writes info to log with console.log.
foreign import info :: String -> Effect Unit

-- | Begin an output group.
-- | Output until the next `groupEnd` will be foldable in this group
foreign import startGroup :: String -> Effect Unit

-- | End an output group.
foreign import endGroup :: String -> Effect Unit

-- | Saves state for current action.
-- | The state can only be retrieved by this action's post job execution.
foreign import saveState :: { name :: String, value :: String } -> Effect Unit

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
