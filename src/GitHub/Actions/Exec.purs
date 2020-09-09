-- | Exports functions from the @actions/exec module provided by GitHub
-- | https://github.com/actions/toolkit/tree/main/packages/exec
module GitHub.Actions.Exec
  ( exec
  , exec'
  , ExecArgs
  , ExecOptions
  , defaultExecOptions
  , ExecListeners
  , defaultExecListeners
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)
import Foreign.Object (Object)
import Node.Buffer (Buffer)
import Node.Stream (Writable)

-- | cwd: working directory.  defaults to current
-- | env: envvar dictionary.  defaults to current process's env
-- | silent: defaults to false
-- | outStream: Defaults to process.stdout
-- | errStream: Defaults to process.stderr
-- | windowsVerbatimArguments: whether to skip quoting/escaping arguments if needed.  defaults to false
-- | failOnStdErr: whether to fail if output to stderr.  defaults to false
-- | ignoreReturnCode: defaults to failing on non zero.  ignore will not fail leaving it up to the caller
-- | delay: How long in ms to wait for STDIO streams to close after the exit event of the process before terminating. defaults to 10000
-- | input: input to write to the process on STDIN
-- | listeners: Listeners for output. Callback functions that will be called on these events
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

-- | Defaults for ExecOptions. Override as needed.
defaultExecOptions :: ExecOptions
defaultExecOptions =
  { cwd: Nothing
  , env: Nothing
  , silent: Nothing
  , outStream: Nothing
  , errStream: Nothing
  , windowsVerbatimArguments: Nothing
  , failOnStdErr: Nothing
  , ignoreReturnCode: Nothing
  , delay: Nothing
  , input: Nothing
  , listeners: Nothing
  }

-- | Listeners for output. Callback functions that will be called on these events
type ExecListeners =
  { stdout :: Maybe (Buffer -> Effect Unit)
  , stderr :: Maybe (Buffer -> Effect Unit)
  , stdline :: Maybe (String -> Effect Unit)
  , errline :: Maybe (String -> Effect Unit)
  , debug :: Maybe (String -> Effect Unit)
  }

-- | Defaults for ExecListeners. Override as needed.
defaultExecListeners :: ExecListeners
defaultExecListeners =
  { stdout: Nothing
  , stderr: Nothing
  , stdline: Nothing
  , errline: Nothing
  , debug: Nothing
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

foreign import exec2Impl2 :: EffectFn2 String JSExecOptions (Promise Number)

foreign import exec3Impl :: EffectFn3 String (Array String) JSExecOptions (Promise Number)

-- | commandLine: command to execute (can include additional args). Must be correctly escaped.
-- | args: optional arguments for tool. Escaping is handled by the lib. Defaults to empty array.
-- | options: optional exec options.  See ExecOptions
type ExecArgs =
  { command :: String
  , args :: Maybe (Array String)
  , options :: Maybe ExecOptions
  }

-- | Executes a command on the command line. Uses defaults for args and options
exec' :: String -> ExceptT Error Aff Number
exec' command = exec { command, args: Nothing, options: Nothing }

-- | Executes a command on the command line, with arguments
-- | Output will be streamed to the live console. Provides return code
exec :: ExecArgs -> ExceptT Error Aff Number
exec =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
  where
  handleOptions { command, args, options } = case args, options of
    Nothing, Nothing -> runEffectFn1 exec1Impl command
    Just a, Nothing -> runEffectFn2 exec2Impl command a
    Nothing, Just o -> runEffectFn2 exec2Impl2 command (toJSExecOptions o)
    Just a, Just o -> runEffectFn3 exec3Impl command a (toJSExecOptions o)
