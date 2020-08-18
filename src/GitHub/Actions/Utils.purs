module GitHub.Actions.Utils where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either)
import Effect.Aff (Aff, try)
import GitHub.Actions.Types (ActionsM, ActionsError)

runActionsM :: forall a. ActionsM a -> Aff (Either ActionsError a)
runActionsM = runExceptT

tryActionsM :: forall a. Aff a -> ActionsM a
tryActionsM = try >>> ExceptT
