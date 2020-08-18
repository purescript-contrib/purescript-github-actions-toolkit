-- | Exports functions from the @actions/core module provided by GitHub
-- | https://github.com/actions/toolkit/tree/main/packages/core
module GitHub.Actions.Core where

import Prelude

import Control.Promise (Promise, fromAff, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)

type InputOptions =
  { required :: Boolean
  }

foreign import exportVariable :: { key :: String, value :: String } -> Effect Unit

foreign import setSecret :: String -> Effect Unit

foreign import addPath :: String -> Effect Unit

foreign import getInputImpl
  :: { name :: String, options :: Nullable InputOptions }
  -> Effect (Nullable String)

-- TODO: handle thrown error
getInput
  :: { name :: String, options :: Maybe InputOptions }
  -> Effect (Maybe String)
getInput { name, options } =
  map toMaybe (getInputImpl { name, options: toNullable options })

foreign import setOutput :: { name :: String, value :: String } -> Effect Unit

foreign import setCommandEcho :: Boolean -> Effect Unit

foreign import setFailed :: String -> Effect Unit

foreign import isDebug :: Effect Boolean

foreign import debug :: String -> Effect Unit

foreign import error :: String -> Effect Unit

foreign import warning :: String -> Effect Unit

foreign import info :: String -> Effect Unit

foreign import startGroup :: String -> Effect Unit

foreign import endGroup :: String -> Effect Unit

foreign import saveState :: { name :: String, value :: String } -> Effect Unit

foreign import getStateImpl :: String -> Effect (Nullable String)

getState :: String -> Effect (Maybe String)
getState = getStateImpl >>> map toMaybe

foreign import groupImpl
  :: forall a
   . { name :: String, fn :: Effect (Promise a) }
  -> Effect (Promise a)

group :: forall a. { name :: String, fn :: Aff a } -> Aff a
group { name, fn } =
  toAffE (groupImpl { name, fn: fromAff fn })
