-- | Exports functions from the @actions/cache module provided by GitHub
-- | https://github.com/actions/toolkit/tree/main/packages/cache
module GitHub.Actions.Cache where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, runEffectFn2, runEffectFn3, runEffectFn4)
import GitHub.Actions.Arguments.Optional (Optional1, Optional2, handleOptional1, handleOptional2)

foreign import restoreCache2Impl :: EffectFn2 (Array String) String (Promise (Nullable String))

foreign import restoreCache3Impl :: EffectFn3 (Array String) String (Array String) (Promise (Nullable String))

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

type RestoreCacheArgs = Optional2 ( paths :: Array String, primaryKey :: String ) "restoreKeys" (Array String) "options" DownloadOptions

restoreCache :: RestoreCacheArgs -> ExceptT Error Aff (Maybe String)
restoreCache =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
    >>> map toMaybe
  where
  handleOptions = handleOptional2
    { required: \{ paths, primaryKey } -> runEffectFn2 restoreCache2Impl paths primaryKey
    , specifyOne: \{ paths, primaryKey, restoreKeys } -> runEffectFn3 restoreCache3Impl paths primaryKey restoreKeys
    , specifyTwo: \{ paths, primaryKey, restoreKeys, options } -> runEffectFn4 restoreCache4Impl paths primaryKey restoreKeys (toJSDownloadOptions options)
    }

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

type SaveCacheArgs = Optional1 ( paths :: Array String, key :: String ) "options" UploadOptions

saveCache :: SaveCacheArgs -> ExceptT Error Aff Number
saveCache =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
  where
  handleOptions = handleOptional1
    { required: \{ paths, key } -> runEffectFn2 saveCache2Impl paths key
    , specifyOne: \{ paths, key, options } -> runEffectFn3 saveCache3Impl paths key (toJSUploadOptions options)
    }
