module GitHub.Actions.ToolCache
  ( downloadTool
  , extract7c
  , extractTar
  , extractXar
  , extractZip
  , cacheDir
  , cacheFile
  , find
  , findAllVersions
  , getManifestFromRepo
  , findFromManifest
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Class (liftEffect)
import GitHub.Actions.Types (ActionsM)
import GitHub.Actions.Utils (tryActionsM)

foreign import downloadToolImpl :: { url :: String, dest :: Nullable String, auth :: Nullable String } -> Effect (Promise (Nullable String))

-- | Download a tool from an url and stream it into a file
downloadTool :: { url :: String, dest :: Maybe String, auth :: Maybe String } -> ActionsM (Maybe String)
downloadTool { url, dest, auth } =
  downloadToolImpl { url, dest: toNullable dest, auth: toNullable auth }
    # toAffE
    # tryActionsM
    # map toMaybe

foreign import extract7cImpl :: { file :: String, dest :: Nullable String, _7zPath :: Nullable String } -> Effect (Promise (Nullable String))

-- | Extract a .7z file
extract7c :: { file :: String, dest :: Maybe String, _7zPath :: Maybe String } -> ActionsM (Maybe String)
extract7c { file, dest, _7zPath } =
  extract7cImpl { file, dest: toNullable dest, _7zPath: toNullable _7zPath }
    # toAffE
    # tryActionsM
    # map toMaybe

foreign import extractTarImpl :: { file :: String, dest :: Nullable String, flags :: Nullable String } -> Effect (Promise (Nullable String))

-- | Extract a compressed tar archive
extractTar :: { file :: String, dest :: Maybe String, flags :: Maybe String } -> ActionsM (Maybe String)
extractTar { file, dest, flags } =
  extractTarImpl { file, dest: toNullable dest, flags: toNullable flags }
    # toAffE
    # tryActionsM
    # map toMaybe

foreign import extractXarImpl :: { file :: String, dest :: Nullable String, flags :: Nullable String } -> Effect (Promise (Nullable String))

-- | Extract a xar compatible archive
extractXar :: { file :: String, dest :: Maybe String, flags :: Maybe String } -> ActionsM (Maybe String)
extractXar { file, dest, flags } =
  extractXarImpl { file, dest: toNullable dest, flags: toNullable flags }
    # toAffE
    # tryActionsM
    # map toMaybe

foreign import extractZipImpl :: { file :: String, dest :: Nullable String } -> Effect (Promise (Nullable String))

-- | Extract a zip
extractZip :: { file :: String, dest :: Maybe String } -> ActionsM (Maybe String)
extractZip { file, dest } =
  extractZipImpl { file, dest: toNullable dest }
    # toAffE
    # tryActionsM
    # map toMaybe

foreign import cacheDirImpl :: { sourceDir :: String, tool :: String, version :: String, arch :: Nullable String } -> Effect (Promise (Nullable String))

-- | Caches a directory and installs it into the tool cacheDir
cacheDir :: { sourceDir :: String, tool :: String, version :: String, arch :: Maybe String } -> ActionsM (Maybe String)
cacheDir { sourceDir, tool, version, arch } =
  cacheDirImpl { sourceDir, tool, version, arch: toNullable arch }
    # toAffE
    # tryActionsM
    # map toMaybe

foreign import cacheFileImpl :: { sourceFile :: String, targetFile :: String, tool :: String, version :: String, arch :: Nullable String } -> Effect (Promise (Nullable String))

-- | Caches a downloaded file (GUID) and installs it
-- | into the tool cache with a given targetName
cacheFile :: { sourceFile :: String, targetFile :: String, tool :: String, version :: String, arch :: Maybe String } -> ActionsM (Maybe String)
cacheFile { sourceFile, targetFile, tool, version, arch } =
  cacheFileImpl { sourceFile, targetFile, tool, version, arch: toNullable arch }
    # toAffE
    # tryActionsM
    # map toMaybe

foreign import findImpl :: { toolName :: String, versionSpec :: String, arch :: Nullable String } -> Effect (Nullable String)

-- | Finds the path to a tool version in the local installed tool cache
find :: { toolName :: String, versionSpec :: String, arch :: Maybe String } -> ActionsM (Maybe String)
find { toolName, versionSpec, arch } =
  findImpl { toolName, versionSpec, arch: toNullable arch }
    # liftEffect
    # tryActionsM
    # map toMaybe

foreign import findAllVersionsImpl :: { toolName :: String, arch :: Nullable String } -> Effect (Array String)

-- | Finds the paths to all versions of a tool that are installed in the local tool cache
findAllVersions :: { toolName :: String, arch :: Maybe String } -> ActionsM (Array String)
findAllVersions { toolName, arch } =
  findAllVersionsImpl { toolName, arch: toNullable arch }
    # liftEffect
    # tryActionsM

type IToolReleaseFileWrapper f =
  { filename :: String
  , platform :: String
  , platform_version :: f String
  , arch :: String
  , download_url :: String
  }

type IToolReleaseFile = IToolReleaseFileWrapper Maybe

type JsIToolReleaseFile = IToolReleaseFileWrapper Nullable

toIToolReleaseFile :: JsIToolReleaseFile -> IToolReleaseFile
toIToolReleaseFile { filename, platform, platform_version, arch, download_url } =
  { filename
  , platform
  , platform_version: toMaybe platform_version
  , arch
  , download_url
  }

fromIToolReleaseFile :: IToolReleaseFile -> JsIToolReleaseFile
fromIToolReleaseFile { filename, platform, platform_version, arch, download_url } =
  { filename
  , platform
  , platform_version: toNullable platform_version
  , arch
  , download_url
  }

type IToolReleaseWrapper f =
  { version :: String
  , stable :: Boolean
  , release_url :: String
  , files :: Array (IToolReleaseFileWrapper f)
  }

type IToolRelease = IToolReleaseWrapper Maybe

type JsIToolRelease = IToolReleaseWrapper Nullable

toIToolRelease :: JsIToolRelease -> IToolRelease
toIToolRelease { version, stable, release_url, files } =
  { version
  , stable
  , release_url
  , files: map toIToolReleaseFile files
  }

fromIToolRelease :: IToolRelease -> JsIToolRelease
fromIToolRelease { version, stable, release_url, files } =
  { version
  , stable
  , release_url
  , files: map fromIToolReleaseFile files
  }

foreign import getManifestFromRepoImpl :: { owner :: String, repo :: String, auth :: Nullable String, branch :: Nullable String } -> Effect (Promise (Array JsIToolRelease))

getManifestFromRepo :: { owner :: String, repo :: String, auth :: Maybe String, branch :: Maybe String } -> ActionsM (Array IToolRelease)
getManifestFromRepo { owner, repo, auth, branch } =
  getManifestFromRepoImpl { owner, repo, auth: toNullable auth, branch: toNullable branch }
    # toAffE
    # tryActionsM
    # map (map toIToolRelease)

foreign import findFromManifestImpl :: { versionSpec :: String, stable :: Boolean, manifest :: Array JsIToolRelease, archFilter :: Nullable String } -> Effect (Promise (Nullable JsIToolRelease))

findFromManifest :: { versionSpec :: String, stable :: Boolean, manifest :: Array IToolRelease, archFilter :: Maybe String } -> ActionsM (Maybe IToolRelease)
findFromManifest { versionSpec, stable, manifest, archFilter } =
  findFromManifestImpl { versionSpec, stable, manifest: map fromIToolRelease manifest, archFilter: toNullable archFilter }
    # toAffE
    # tryActionsM
    # map (toMaybe >>> map toIToolRelease)
