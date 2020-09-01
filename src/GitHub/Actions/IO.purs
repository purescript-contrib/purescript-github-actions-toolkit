-- | Exports functions from the @actions/io module provided by GitHub
-- | https://github.com/actions/toolkit/tree/main/packages/io
module GitHub.Actions.IO
  ( CopyOptions
  , defaultCopyOptions
  , MoveOptions
  , defaultMoveOptions
  , mv
  , mv'
  , MvArgs
  , cp
  , cp'
  , CpArgs
  , mkdirP
  , MkdirPArgs
  , which
  , which'
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

-- | Whether to recursively copy all subdirectories. Defaults to false
-- | Whether to overwrite existing files in the destination. Defaults to true
type CopyOptions =
  { recursive :: Maybe Boolean
  , force :: Maybe Boolean
  }

-- | Defaults for CopyOptions. Override as needed.
defaultCopyOptions :: CopyOptions
defaultCopyOptions =
  { recursive: Nothing
  , force: Nothing
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

-- | Whether to overwrite existing files in the destination. Defaults to true
type MoveOptions =
  { force :: Maybe Boolean
  }

-- | Defaults for MoveOptions. Override as needed.
defaultMoveOptions :: MoveOptions
defaultMoveOptions =
  { force: Nothing
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

-- | source: source path
-- | dest: destination path
-- | options: See CopyOptions
type CpArgs =
  { source :: FilePath
  , dest :: FilePath
  , options :: Maybe CopyOptions
  }

-- | Copies a file or folder
cp' :: { source :: FilePath, dest :: FilePath } -> ExceptT Error Aff Unit
cp' { source, dest } = cp { source, dest, options: Nothing }

-- | Copies a file or folder
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

-- | source: source path
-- | dest: destination path
-- | options: See MoveOptions
type MvArgs =
  { source :: FilePath
  , dest :: FilePath
  , options :: Maybe MoveOptions
  }

-- | Moves a path
mv' :: { source :: FilePath, dest :: FilePath } -> ExceptT Error Aff Unit
mv' { source, dest } = mv { source, dest, options: Nothing }

-- | Moves a path
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

-- | inputPath: path to remove
type RmRFArgs =
  { inputPath :: FilePath
  }

-- | Remove a path recursively with force
rmRF :: RmRFArgs -> ExceptT Error Aff Unit
rmRF { inputPath } =
  runEffectFn1 rmRFImpl inputPath
    # toAffE
    # (try >>> ExceptT)
    # void

foreign import mkdirPImpl :: EffectFn1 String (Promise VoidReturn)

-- | fsPath: path to create
type MkdirPArgs =
  { fsPath :: FilePath
  }

-- | Make a directory.  Creates the full path with folders in between. Will throw if it fails
mkdirP :: MkdirPArgs -> ExceptT Error Aff Unit
mkdirP { fsPath } =
  runEffectFn1 mkdirPImpl fsPath
    # toAffE
    # (try >>> ExceptT)
    # void

foreign import which2Impl :: EffectFn1 String (Promise String)

foreign import which3Impl :: EffectFn2 String Boolean (Promise String)

-- | tool: name of the tool
-- | check: whether to check if tool exists. Defaults to false
type WhichArgs =
  { tool :: String
  , check :: Maybe Boolean
  }

which' :: String -> ExceptT Error Aff FilePath
which' tool = which { tool, check: Nothing }

-- | Returns path of a tool had the tool actually been invoked.  Resolves via paths. If you check and the tool does not exist, it will throw.
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
