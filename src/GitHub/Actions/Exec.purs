-- | Exports functions from the @actions/exec module provided by GitHub
-- | https://github.com/actions/toolkit/tree/main/packages/exec
module GitHub.Actions.Exec
  ( exec
  , ExecArgs
  , ExecOptions
  , ExecListeners
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)
import Foreign.Object (Object)
import GitHub.Actions.Arguments.Optional (Optional2, handleOptional2)
import GitHub.Actions.Utils (tryActionsM)
import Node.Buffer (Buffer)
import Node.Stream (Writable)

type ExecOptions =
  { cwd :: Maybe String
  , env :: Maybe (Object String)
  , silent :: Maybe Boolean
  , outStream :: Maybe (Writable ())
  , errStream :: Maybe (Writable ())
  , windowsVerbatimArguments :: Maybe Boolean
  , failOnStdErr :: Maybe Boolean
  , ignoreReturnCode :: Maybe Boolean
  , delay :: Maybe Number
  , input :: Maybe Buffer
  , listeners :: Maybe ExecListeners
  }

type ExecListeners =
  { stdout :: Maybe (Buffer -> Effect Unit)
  , stderr :: Maybe (Buffer -> Effect Unit)
  , stdline :: Maybe (String -> Effect Unit)
  , errline :: Maybe (String -> Effect Unit)
  , debug :: Maybe (String -> Effect Unit)
  }

type JSExecOptions =
  { cwd :: Nullable String
  , env :: Nullable (Object String)
  , silent :: Nullable Boolean
  , outStream :: Nullable (Writable ())
  , errStream :: Nullable (Writable ())
  , windowsVerbatimArguments :: Nullable Boolean
  , failOnStdErr :: Nullable Boolean
  , ignoreReturnCode :: Nullable Boolean
  , delay :: Nullable Number
  , input :: Nullable Buffer
  , listeners :: Nullable JSExecListeners
  }

type JSExecListeners =
  { stdout :: Nullable (EffectFn1 Buffer Unit)
  , stderr :: Nullable (EffectFn1 Buffer Unit)
  , stdline :: Nullable (EffectFn1 String Unit)
  , errline :: Nullable (EffectFn1 String Unit)
  , debug :: Nullable (EffectFn1 String Unit)
  }

toJSExecOptions :: ExecOptions -> JSExecOptions
toJSExecOptions
  { cwd
  , env
  , silent
  , outStream
  , errStream
  , windowsVerbatimArguments
  , failOnStdErr
  , ignoreReturnCode
  , delay
  , input
  , listeners
  } =
  { cwd: toNullable cwd
  , env: toNullable env
  , silent: toNullable silent
  , outStream: toNullable outStream
  , errStream: toNullable errStream
  , windowsVerbatimArguments: toNullable windowsVerbatimArguments
  , failOnStdErr: toNullable failOnStdErr
  , ignoreReturnCode: toNullable ignoreReturnCode
  , delay: toNullable delay
  , input: toNullable input
  , listeners: toNullable $ map toJSExecListeners listeners
  }

toJSExecListeners :: ExecListeners -> JSExecListeners
toJSExecListeners
  { stdout
  , stderr
  , stdline
  , errline
  , debug
  } =
  { stdout: toNullable $ map mkEffectFn1 stdout
  , stderr: toNullable $ map mkEffectFn1 stderr
  , stdline: toNullable $ map mkEffectFn1 stdline
  , errline: toNullable $ map mkEffectFn1 errline
  , debug: toNullable $ map mkEffectFn1 debug
  }

foreign import exec1Impl :: EffectFn1 String (Promise Number)

foreign import exec2Impl :: EffectFn2 String (Array String) (Promise Number)

foreign import exec3Impl :: EffectFn3 String (Array String) JSExecOptions (Promise Number)

type ExecArgs = Optional2 ( command :: String ) "args" (Array String) "options" ExecOptions

-- | Executes a command on the command line, with arguments
exec :: ExecArgs -> ExceptT Error Aff { succeeded :: Boolean }
exec =
  handleOptions
    >>> toAffE
    >>> tryActionsM
    >>> map ((_ == 0.0) >>> { succeeded: _ })
  where
  handleOptions = handleOptional2
    { required: \{ command } -> runEffectFn1 exec1Impl command
    , specifyOne: \{ command, args } -> runEffectFn2 exec2Impl command args
    , specifyTwo: \{ command, args, options } -> runEffectFn3 exec3Impl command args (toJSExecOptions options)
    }
