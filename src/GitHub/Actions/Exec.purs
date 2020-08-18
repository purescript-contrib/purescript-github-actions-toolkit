module GitHub.Actions.Exec
  ( exec
  , ExecOptions
  , ExecOptionsWrapper
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..))
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect.Aff (try)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Foreign.Object (Object)
import GitHub.Actions.Types (ActionsM)

-- TODO: skipped stream/buffer stuff
type ExecOptionsWrapper f =
  { cwd :: f String
  , env :: f (Object String)
  , silent :: f Boolean
  , windowsVerbatimArguments :: f Boolean
  , failOnStdErr :: f Boolean
  , ignoreReturnCode :: f Boolean
  , delay :: f Number
  }

type ExecOptions = ExecOptionsWrapper Maybe

type JSExecOptions = ExecOptionsWrapper Nullable

mkNullableExecOptions :: ExecOptions -> JSExecOptions
mkNullableExecOptions
  { cwd
  , env
  , silent
  , windowsVerbatimArguments
  , failOnStdErr
  , ignoreReturnCode
  , delay
  } =
  { cwd: toNullable cwd
  , env: toNullable env
  , silent: toNullable silent
  , windowsVerbatimArguments: toNullable windowsVerbatimArguments
  , failOnStdErr: toNullable failOnStdErr
  , ignoreReturnCode: toNullable ignoreReturnCode
  , delay: toNullable delay
  }

foreign import execImpl :: EffectFn3 String (Array String) (Nullable JSExecOptions) (Promise Number)

-- | Executes a command on the command line, with arguments
exec :: String -> Array String -> Maybe ExecOptions -> ActionsM { succeeded :: Boolean }
exec command args options =
  runEffectFn3 execImpl command args (toNullable (map mkNullableExecOptions options))
    # toAffE
    # try >>> ExceptT
    # map ((_ == 0.0) >>> { succeeded: _ })
