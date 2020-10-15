-- | Exports functions from the @actions/glob module provided by GitHub
-- | https://github.com/actions/toolkit/tree/main/packages/glob
module GitHub.Actions.Glob
  ( Globber
  , GlobOptions(..)
  , getSearchPaths
  , glob
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff, Error, try)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

-- | Used to match files and directories.
foreign import data Globber :: Type

-- | Options to control globbing behavior.
type GlobOptions =
  { followSymbolicLinks :: Maybe Boolean
  , implicitDescendants :: Maybe Boolean
  , omitBrokenSymbolicLinks :: Maybe Boolean
  }

-- | Defaults for `GlobOptions`.
defaultGlobOptions :: GlobOptions
defaultGlobOptions =
  { followSymbolicLinks: Just true
  , implicitDescendants: Just true
  , omitBrokenSymbolicLinks: Just true
  }

type JSGlobOptions =
  { followSymbolicLinks :: Nullable Boolean
  , implicitDescendants :: Nullable Boolean
  , omitBrokenSymbolicLinks :: Nullable Boolean
  }

toJSGlobOptions :: GlobOptions -> JSGlobOptions
toJSGlobOptions opts =
  { followSymbolicLinks: toNullable opts.followSymbolicLinks
  , implicitDescendants: toNullable opts.implicitDescendants
  , omitBrokenSymbolicLinks: toNullable opts.omitBrokenSymbolicLinks
  }

foreign import createImpl :: EffectFn2 String (Nullable (JSGlobOptions)) (Promise Globber)

-- | Constructs a globber.
create :: Maybe GlobOptions -> String -> ExceptT Error Aff Globber
create opts pattern =
  runEffectFn2 createImpl pattern jsOpts
    # toAffE
    # (try >>> ExceptT)
  where
  jsOpts = toNullable $ map toJSGlobOptions $ opts

-- | Returns the search path preceding the first glob segment, from each pattern.
-- | Duplicates and descendants of other paths are filtered out.
foreign import getSearchPaths :: Globber -> Effect (Array String)

foreign import globImpl :: EffectFn1 Globber (Promise (Array String))

-- | Returns files and directories matching the glob patterns.
-- |
-- | Order of the results is not guaranteed.
glob :: Globber -> ExceptT Error Aff (Array String)
glob globber =
  runEffectFn1 globImpl globber
    # toAffE
    # (try >>> ExceptT)
