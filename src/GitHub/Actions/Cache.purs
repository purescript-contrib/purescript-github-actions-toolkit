-- | Exports functions from the @actions/cache module provided by GitHub
-- | https://github.com/actions/toolkit/tree/main/packages/cache
module GitHub.Actions.Cache
  ( DownloadOptions
  , defaultDownloadOptions
  , RestoreCacheArgs
  , SaveCacheArgs
  , UploadOptions
  , defaultUploadOptions
  , restoreCache
  , restoreCache'
  , saveCache
  , saveCache'
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

-- | useAzureSdk: Indicates whether to use the Azure Blob SDK to download caches that are stored on Azure Blob Storage to improve reliability and performance
-- | useAzureSdk default: true
-- | downloadConcurrency: Number of parallel downloads (this option only applies when using the Azure SDK)
-- | downloadConcurrency default: 8
-- | Maximum time for each download request, in milliseconds (this option only applies when using the Azure SDK)
-- | timeoutInMs default: 30000
type DownloadOptions =
  { useAzureSdk :: Maybe Boolean
  , downloadConcurrency :: Maybe Boolean
  , timeoutInMs :: Maybe Number
  }

-- | A default set of Download Options. Override as needed
defaultDownloadOptions :: DownloadOptions
defaultDownloadOptions =
  { useAzureSdk: Nothing
  , downloadConcurrency: Nothing
  , timeoutInMs: Nothing
  }

toJSDownloadOptions :: DownloadOptions -> JSDownloadOptions
toJSDownloadOptions { useAzureSdk, downloadConcurrency, timeoutInMs } =
  { useAzureSdk: toNullable useAzureSdk
  , downloadConcurrency: toNullable downloadConcurrency
  , timeoutInMs: toNullable timeoutInMs
  }

-- | paths: a list of file paths to restore from the cache
-- | primaryKey: an explicit key for restoring the cache
-- | restoreKeys: an optional ordered list of keys to use for restoring the cache if no cache hit occurred for key
-- | downloadOptions: cache download options
type RestoreCacheArgs =
  { paths :: Array String
  , primaryKey :: String
  , restoreKeys :: Maybe (Array String)
  , options :: Maybe DownloadOptions
  }

-- | Restores a cache based on primaryKey to the paths provided. Function returns the cache key for cache hit.
-- | https://github.com/actions/toolkit/tree/main/packages/cache#restore-cache
restoreCache' :: { paths :: Array String, primaryKey :: String } -> ExceptT Error Aff (Maybe String)
restoreCache' { paths, primaryKey } = restoreCache { paths, primaryKey, restoreKeys: Nothing, options: Nothing }

-- | Restores a cache based on primaryKey and restoreKeys to the paths provided. Function returns the cache key for cache hit.
-- | See https://github.com/actions/toolkit/tree/main/packages/cache#save-cache
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

-- | uploadConcurrency: Number of parallel cache upload
-- | uploadConcurrency default: 4
-- | uploadChunkSize: Maximum chunk size in bytes for cache upload
-- | uploadChunkSize default: 32MB
type UploadOptions =
  { uploadConcurrency :: Maybe Number
  , uploadChunkSize :: Maybe Number
  }

-- | A default set of Upload Options. Override as needed
defaultUploadOptions :: UploadOptions
defaultUploadOptions =
  { uploadConcurrency: Nothing
  , uploadChunkSize: Nothing
  }

toJSUploadOptions :: UploadOptions -> JSUploadOptions
toJSUploadOptions { uploadConcurrency, uploadChunkSize } =
  { uploadConcurrency: toNullable uploadConcurrency
  , uploadChunkSize: toNullable uploadChunkSize
  }

-- | paths: a list of file paths to be cached
-- | key: an explicit key for restoring the cache
-- | options: cache upload options
type SaveCacheArgs =
  { paths :: Array String
  , key :: String
  , options :: Maybe UploadOptions
  }

-- | Saves a cache containing the files in paths using the key provided. The files would be compressed using zstandard compression algorithm if zstd is installed, otherwise gzip is used. Function returns the cache id if the cache was saved succesfully and throws an error if cache upload fails.
-- | See https://github.com/actions/toolkit/tree/main/packages/cache#save-cache
saveCache' :: { paths :: Array String, key :: String } -> ExceptT Error Aff Number
saveCache' { paths, key } = saveCache { paths, key, options: Nothing }

-- | Saves a cache containing the files in paths using the key provided. The files would be compressed using zstandard compression algorithm if zstd is installed, otherwise gzip is used. Function returns the cache id if the cache was saved succesfully and throws an error if cache upload fails.
-- | See https://github.com/actions/toolkit/tree/main/packages/cache#save-cache
saveCache :: SaveCacheArgs -> ExceptT Error Aff Number
saveCache =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
  where
  handleOptions { paths, key, options } = case options of
    Nothing -> runEffectFn2 saveCache2Impl paths key
    Just opts -> runEffectFn3 saveCache3Impl paths key (toJSUploadOptions opts)
