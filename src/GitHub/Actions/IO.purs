-- | Exports functions from the @actions/io module provided by GitHub
-- | https://github.com/actions/toolkit/tree/main/packages/io
module GitHub.Actions.IO
  ( CopyOptions
  , MoveOptions
  , mv
  , MvArgs
  , cp
  , CpArgs
  , mkdirP
  , MkdirPArgs
  , which
  , WhichArgs
  , rmRF
  , RmRFArgs
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Except.Trans (ExceptT(..), throwError)
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect.Aff (Aff)
import Effect.Exception (Error, error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
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

type CpArgs =
  { source :: FilePath
  , dest :: FilePath
  , options :: Maybe CopyOptions
  }

cp :: CpArgs -> ExceptT Error Aff Unit
cp =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
    >>> void
  where
  handleOptions { source, dest, options } = case options of
    Nothing -> runEffectFn2 cp2Impl source dest
    Just o -> runEffectFn3 cp3Impl source dest (toJSCopyOptions o)

foreign import mv2Impl :: EffectFn2 String String (Promise VoidReturn)

foreign import mv3Impl :: EffectFn3 String String JSMoveOptions (Promise VoidReturn)

type MvArgs =
  { source :: FilePath
  , dest :: FilePath
  , options :: Maybe MoveOptions
  }

mv :: MvArgs -> ExceptT Error Aff Unit
mv =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
    >>> void
  where
  handleOptions { source, dest, options } = case options of
    Nothing -> runEffectFn2 mv2Impl source dest
    Just o -> runEffectFn3 mv3Impl source dest (toJSMoveOptions o)

foreign import rmRFImpl :: EffectFn1 String (Promise VoidReturn)

type RmRFArgs =
  { inputPath :: FilePath
  }

rmRF :: RmRFArgs -> ExceptT Error Aff Unit
rmRF { inputPath } =
  runEffectFn1 rmRFImpl inputPath
    # toAffE
    # (try >>> ExceptT)
    # void

foreign import mkdirPImpl :: EffectFn1 String (Promise VoidReturn)

type MkdirPArgs =
  { fsPath :: FilePath
  }

mkdirP :: MkdirPArgs -> ExceptT Error Aff Unit
mkdirP { fsPath } =
  runEffectFn1 mkdirPImpl fsPath
    # toAffE
    # (try >>> ExceptT)
    # void

foreign import which2Impl :: EffectFn1 String (Promise String)

foreign import which3Impl :: EffectFn2 String Boolean (Promise String)

type WhichArgs =
  { tool :: String
  , check :: Maybe Boolean
  }

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
  handleOptions { tool, check } = case check of
    Nothing -> runEffectFn1 which2Impl tool
    Just c -> runEffectFn2 which3Impl tool c
