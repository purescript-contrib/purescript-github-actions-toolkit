module GitHub.Actions.Exec
  ( exec
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
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, runEffectFn3)
import Foreign.Object (Object)
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

foreign import execImpl :: EffectFn3 String (Array String) (Nullable JSExecOptions) (Promise Number)

-- | Executes a command on the command line, with arguments
exec
  :: String
  -> Array String
  -> Maybe ExecOptions
  -> ExceptT Error Aff { succeeded :: Boolean }
exec command args options =
  runEffectFn3 execImpl command args (toNullable (map toJSExecOptions options))
    # toAffE
    # tryActionsM
    # map ((_ == 0.0) >>> { succeeded: _ })
