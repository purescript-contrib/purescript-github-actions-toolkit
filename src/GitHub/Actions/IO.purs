-- | Exports functions from the @actions/io module provided by GitHub
-- | https://github.com/actions/toolkit/tree/main/packages/io
module GitHub.Actions.IO where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Except.Trans (ExceptT(..), throwError)
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect.Aff (Aff)
import Effect.Exception (Error, error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import GitHub.Actions.Arguments.Optional (Optional1, Required, handleOptional1, handleRequired)
import GitHub.Actions.Types (Tool)
import Node.Path (FilePath)

type CopyOptions =
  { recursive :: Maybe Boolean
  , force :: Maybe Boolean
  }

type JSCopyOptions =
  { recursive :: Nullable Boolean
  , force :: Nullable Boolean
  }

toJSCopyOptions :: CopyOptions -> JSCopyOptions
toJSCopyOptions { recursive, force } =
  { recursive: toNullable recursive
  , force: toNullable force
  }

type MoveOptions =
  { force :: Maybe Boolean
  }

type JSMoveOptions =
  { force :: Nullable Boolean
  }

toJSMoveOptions :: MoveOptions -> JSMoveOptions
toJSMoveOptions { force } =
  { force: toNullable force
  }

foreign import data VoidReturn :: Type

foreign import cp2Impl :: EffectFn2 String String (Promise VoidReturn)

foreign import cp3Impl :: EffectFn3 String String JSCopyOptions (Promise VoidReturn)

type CpArgs = Optional1 ( source :: FilePath, dest :: FilePath ) "options" CopyOptions

cp :: CpArgs -> ExceptT Error Aff Unit
cp =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
    >>> void
  where
  handleOptions = handleOptional1
    { required: \{ source, dest } -> runEffectFn2 cp2Impl source dest
    , specifyOne: \{ source, dest, options } -> runEffectFn3 cp3Impl source dest (toJSCopyOptions options)
    }

foreign import mv2Impl :: EffectFn2 String String (Promise VoidReturn)

foreign import mv3Impl :: EffectFn3 String String JSMoveOptions (Promise VoidReturn)

type MvArgs = Optional1 ( source :: FilePath, dest :: FilePath ) "options" MoveOptions

mv :: MvArgs -> ExceptT Error Aff Unit
mv =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
    >>> void
  where
  handleOptions = handleOptional1
    { required: \{ source, dest } -> runEffectFn2 mv2Impl source dest
    , specifyOne: \{ source, dest, options } -> runEffectFn3 mv3Impl source dest (toJSMoveOptions options)
    }

foreign import rmRFImpl :: EffectFn1 String (Promise VoidReturn)

type RmRFArgs = Required ( inputPath :: FilePath )

rmRF :: RmRFArgs -> ExceptT Error Aff Unit
rmRF =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
    >>> void
  where
  handleOptions = handleRequired
    { required: \{ inputPath } -> runEffectFn1 rmRFImpl inputPath
    }

foreign import mkdirPImpl :: EffectFn1 String (Promise VoidReturn)

type MkdirPArgs = Required ( fsPath :: FilePath )

mkdirP :: MkdirPArgs -> ExceptT Error Aff Unit
mkdirP =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
    >>> void
  where
  handleOptions = handleRequired
    { required: \{ fsPath } -> runEffectFn1 mkdirPImpl fsPath
    }

foreign import which2Impl :: EffectFn1 String (Promise String)

foreign import which3Impl :: EffectFn2 String Boolean (Promise String)

type WhichArgs = Optional1 ( tool :: Tool ) "check" Boolean

which :: WhichArgs -> ExceptT Error Aff FilePath
which =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
    >=> \filePath -> do
      if (filePath == "") then
        throwError (error "Tool not found")
      else
        pure filePath
  where
  handleOptions = handleOptional1
    { required: \{ tool } -> runEffectFn1 which2Impl tool
    , specifyOne: \{ tool, check } -> runEffectFn2 which3Impl tool check
    }
