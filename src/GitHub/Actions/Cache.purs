-- | Exports functions from the @actions/cache module provided by GitHub
-- | https://github.com/actions/toolkit/tree/main/packages/cache
module GitHub.Actions.Cache
  ( DownloadOptions
  , RestoreCacheArgs
  , SaveCacheArgs
  , UploadOptions
  , restoreCache
  , saveCache
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, runEffectFn2, runEffectFn3, runEffectFn4)

foreign import restoreCache2Impl :: EffectFn2 (Array String) String (Promise (Nullable String))

foreign import restoreCache3Impl :: EffectFn3 (Array String) String (Array String) (Promise (Nullable String))

foreign import restoreCache3Impl2 :: EffectFn3 (Array String) String JSDownloadOptions (Promise (Nullable String))

foreign import restoreCache4Impl :: EffectFn4 (Array String) String (Array String) JSDownloadOptions (Promise (Nullable String))

type JSDownloadOptions =
  { useAzureSdk :: Nullable Boolean
  , downloadConcurrency :: Nullable Boolean
  , timeoutInMs :: Nullable Number
  }

type DownloadOptions =
  { useAzureSdk :: Maybe Boolean
  , downloadConcurrency :: Maybe Boolean
  , timeoutInMs :: Maybe Number
  }

toJSDownloadOptions :: DownloadOptions -> JSDownloadOptions
toJSDownloadOptions { useAzureSdk, downloadConcurrency, timeoutInMs } =
  { useAzureSdk: toNullable useAzureSdk
  , downloadConcurrency: toNullable downloadConcurrency
  , timeoutInMs: toNullable timeoutInMs
  }

type RestoreCacheArgs =
  { paths :: Array String
  , primaryKey :: String
  , restoreKeys :: Maybe (Array String)
  , options :: Maybe DownloadOptions
  }

restoreCache :: RestoreCacheArgs -> ExceptT Error Aff (Maybe String)
restoreCache =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
    >>> map toMaybe
  where
  handleOptions { paths, primaryKey, restoreKeys, options } = case restoreKeys, options of
    Nothing, Nothing -> runEffectFn2 restoreCache2Impl paths primaryKey
    Just restKeys, Nothing -> runEffectFn3 restoreCache3Impl paths primaryKey restKeys
    Nothing, Just opts -> runEffectFn3 restoreCache3Impl2 paths primaryKey (toJSDownloadOptions opts)
    Just restKeys, Just opts -> runEffectFn4 restoreCache4Impl paths primaryKey restKeys (toJSDownloadOptions opts)

foreign import saveCache2Impl :: EffectFn2 (Array String) String (Promise Number)

foreign import saveCache3Impl :: EffectFn3 (Array String) String JSUploadOptions (Promise Number)

type JSUploadOptions =
  { uploadConcurrency :: Nullable Number
  , uploadChunkSize :: Nullable Number
  }

type UploadOptions =
  { uploadConcurrency :: Maybe Number
  , uploadChunkSize :: Maybe Number
  }

toJSUploadOptions :: UploadOptions -> JSUploadOptions
toJSUploadOptions { uploadConcurrency, uploadChunkSize } =
  { uploadConcurrency: toNullable uploadConcurrency
  , uploadChunkSize: toNullable uploadChunkSize
  }

type SaveCacheArgs =
  { paths :: Array String
  , key :: String
  , options :: Maybe UploadOptions
  }

saveCache :: SaveCacheArgs -> ExceptT Error Aff Number
saveCache =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
  where
  handleOptions { paths, key, options } = case options of
    Nothing -> runEffectFn2 saveCache2Impl paths key
    Just opts -> runEffectFn3 saveCache3Impl paths key (toJSUploadOptions opts)
