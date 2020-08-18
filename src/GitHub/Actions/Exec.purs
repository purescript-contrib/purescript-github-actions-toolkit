module GitHub.Actions.Exec
  (exec
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Foreign.Object (Object)

-- TODO: skipped stream/buffer stuff
type ExecOptions f =
  { cwd :: f String
  , env :: f (Object String)
  , silent :: f Boolean
  , windowsVerbatimArguments :: f Boolean
  , failOnStdErr :: f Boolean
  , ignoreReturnCode :: f Boolean
  , delay :: f Number
  }

mkNullableExecOptions :: ExecOptions Maybe -> ExecOptions Nullable
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

foreign import execImpl :: EffectFn3 String (Array String) (Nullable (ExecOptions Nullable)) (Promise Number)

-- | Executes a command on the command line, with arguments
exec :: String -> Array String -> Maybe (ExecOptions Maybe) -> Aff { succeeded :: Boolean }
exec command args options =
  runEffectFn3 execImpl command args (toNullable (map mkNullableExecOptions options))
    # toAffE
    # map ((_ == 0.0) >>> { succeeded: _ })
