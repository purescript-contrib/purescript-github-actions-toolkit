module GitHub.Actions.Types where

import Control.Monad.Except.Trans (ExceptT)
import Effect.Aff (Aff)
import Effect.Exception (Error)

type ActionsError = Error

type ActionsM a = ExceptT ActionsError Aff a

type Tool = String
