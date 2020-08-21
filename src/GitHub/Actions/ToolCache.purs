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
  , IToolRelease
  , IToolReleaseFile
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT, throwError)
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Version (Version)
import Data.Version as Version
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import GitHub.Actions.Types (Tool)
import GitHub.Actions.Utils (tryActionsM)
import Node.Path (FilePath)

foreign import downloadToolImpl :: { url :: String, dest :: Nullable FilePath, auth :: Nullable String } -> Effect (Promise (Nullable String))

-- | Download a tool from an url and stream it into a file
downloadTool
  :: { url :: String, dest :: Maybe FilePath, auth :: Maybe String }
  -> ExceptT Error Aff String
downloadTool { url, dest, auth } = do
  mbPath <- map toMaybe $ tryActionsM $ toAffE $ downloadToolImpl { url, dest: toNullable dest, auth: toNullable auth }
  case mbPath of
    Nothing -> throwError (error "Failed to download tool")
    Just path -> pure path

foreign import extract7cImpl :: { file :: FilePath, dest :: Nullable FilePath, _7zPath :: Nullable String } -> Effect (Promise (Nullable String))

-- | Extract a .7z file
extract7c :: { file :: FilePath, dest :: Maybe FilePath, _7zPath :: Maybe FilePath } -> ExceptT Error Aff String
extract7c { file, dest, _7zPath } = do
  mbPath <- map toMaybe $ tryActionsM $ toAffE $ extract7cImpl { file, dest: toNullable dest, _7zPath: toNullable _7zPath }
  case mbPath of
    Nothing -> throwError (error "Failed to extract .7z file")
    Just path -> pure path

foreign import extractTarImpl :: { file :: FilePath, dest :: Nullable FilePath, flags :: Nullable String } -> Effect (Promise (Nullable FilePath))

-- | Extract a compressed tar archive
extractTar :: { file :: FilePath, dest :: Maybe FilePath, flags :: Maybe String } -> ExceptT Error Aff FilePath
extractTar { file, dest, flags } = do
  mbPath <- map toMaybe $ tryActionsM $ toAffE $ extractTarImpl { file, dest: toNullable dest, flags: toNullable flags }
  case mbPath of
    Nothing -> throwError (error "failed to extract tar")
    Just path -> pure path

foreign import extractXarImpl :: { file :: FilePath, dest :: Nullable FilePath, flags :: Nullable String } -> Effect (Promise (Nullable FilePath))

-- | Extract a xar compatible archive
extractXar :: { file :: FilePath, dest :: Maybe FilePath, flags :: Maybe String } -> ExceptT Error Aff (Maybe FilePath)
extractXar { file, dest, flags } =
  extractXarImpl { file, dest: toNullable dest, flags: toNullable flags }
    # toAffE
    # tryActionsM
    # map toMaybe

foreign import extractZipImpl :: { file :: FilePath, dest :: Nullable FilePath } -> Effect (Promise (Nullable FilePath))

-- | Extract a zip
extractZip :: { file :: FilePath, dest :: Maybe FilePath } -> ExceptT Error Aff (Maybe FilePath)
extractZip { file, dest } =
  extractZipImpl { file, dest: toNullable dest }
    # toAffE
    # tryActionsM
    # map toMaybe

foreign import cacheDirImpl :: { sourceDir :: FilePath, tool :: Tool, version :: String, arch :: Nullable String } -> Effect (Promise (Nullable FilePath))

-- | Caches a directory and installs it into the tool cacheDir
cacheDir :: { sourceDir :: String, tool :: Tool, version :: Version, arch :: Maybe String } -> ExceptT Error Aff (Maybe String)
cacheDir { sourceDir, tool, version, arch } =
  cacheDirImpl { sourceDir, tool, version: Version.showVersion version, arch: toNullable arch }
    # toAffE
    # tryActionsM
    # map toMaybe

foreign import cacheFileImpl :: { sourceFile :: FilePath, targetFile :: FilePath, tool :: Tool, version :: String, arch :: Nullable String } -> Effect (Promise (Nullable FilePath))

-- | Caches a downloaded file (GUID) and installs it
-- | into the tool cache with a given targetName
cacheFile :: { sourceFile :: FilePath, targetFile :: FilePath, tool :: Tool, version :: Version, arch :: Maybe String } -> ExceptT Error Aff FilePath
cacheFile { sourceFile, targetFile, tool, version, arch } = do
  mbPath <- map toMaybe $ tryActionsM $ toAffE $ cacheFileImpl { sourceFile, targetFile, tool, version: Version.showVersion version, arch: toNullable arch }
  case mbPath of
    Nothing -> throwError (error "Failed to cache file")
    Just path -> pure path

foreign import findImpl :: { toolName :: Tool, versionSpec :: String, arch :: Nullable String } -> Effect (Nullable FilePath)

-- | Finds the path to a tool version in the local installed tool cache
find :: { toolName :: Tool, versionSpec :: String, arch :: Maybe String } -> ExceptT Error Aff (Maybe FilePath)
find { toolName, versionSpec, arch } =
  findImpl { toolName, versionSpec, arch: toNullable arch }
    # liftEffect
    # tryActionsM
    # map toMaybe

foreign import findAllVersionsImpl :: { toolName :: Tool, arch :: Nullable String } -> Effect (Array FilePath)

-- | Finds the paths to all versions of a tool that are installed in the local tool cache
findAllVersions :: { toolName :: Tool, arch :: Maybe String } -> ExceptT Error Aff (Array FilePath)
findAllVersions { toolName, arch } =
  findAllVersionsImpl { toolName, arch: toNullable arch }
    # liftEffect
    # tryActionsM

type IToolReleaseFile =
  { filename :: String
  , platform :: String
  , platformVersion :: Maybe String
  , arch :: String
  , downloadUrl :: String
  }

type JSIToolReleaseFile =
  { filename :: String
  , platform :: String
  , platform_version :: Nullable String
  , arch :: String
  , download_url :: String
  }

type IToolRelease =
  { version :: String
  , stable :: Boolean
  , releaseUrl :: String
  , files :: Array IToolReleaseFile
  }

type JSIToolRelease =
  { version :: String
  , stable :: Boolean
  , release_url :: String
  , files :: Array JSIToolReleaseFile
  }

toIToolReleaseFile :: JSIToolReleaseFile -> IToolReleaseFile
toIToolReleaseFile { filename, platform, platform_version, arch, download_url } =
  { filename
  , platform
  , platformVersion: toMaybe platform_version
  , arch
  , downloadUrl: download_url
  }

fromIToolReleaseFile :: IToolReleaseFile -> JSIToolReleaseFile
fromIToolReleaseFile { filename, platform, platformVersion, arch, downloadUrl } =
  { filename
  , platform
  , platform_version: toNullable platformVersion
  , arch
  , download_url: downloadUrl
  }

toIToolRelease :: JSIToolRelease -> IToolRelease
toIToolRelease { version, stable, release_url, files } =
  { version
  , stable
  , releaseUrl: release_url
  , files: map toIToolReleaseFile files
  }

fromIToolRelease :: IToolRelease -> JSIToolRelease
fromIToolRelease { version, stable, releaseUrl, files } =
  { version
  , stable
  , release_url: releaseUrl
  , files: map fromIToolReleaseFile files
  }

foreign import getManifestFromRepoImpl :: { owner :: String, repo :: String, auth :: Nullable String, branch :: Nullable String } -> Effect (Promise (Array JSIToolRelease))

getManifestFromRepo :: { owner :: String, repo :: String, auth :: Maybe String, branch :: Maybe String } -> ExceptT Error Aff (Array IToolRelease)
getManifestFromRepo { owner, repo, auth, branch } =
  getManifestFromRepoImpl { owner, repo, auth: toNullable auth, branch: toNullable branch }
    # toAffE
    # tryActionsM
    # map (map toIToolRelease)

foreign import findFromManifestImpl :: { versionSpec :: String, stable :: Boolean, manifest :: Array JSIToolRelease, archFilter :: Nullable String } -> Effect (Promise (Nullable JSIToolRelease))

findFromManifest :: { versionSpec :: String, stable :: Boolean, manifest :: Array IToolRelease, archFilter :: Maybe String } -> ExceptT Error Aff (Maybe IToolRelease)
findFromManifest { versionSpec, stable, manifest, archFilter } =
  findFromManifestImpl { versionSpec, stable, manifest: map fromIToolRelease manifest, archFilter: toNullable archFilter }
    # toAffE
    # tryActionsM
    # map (toMaybe >>> map toIToolRelease)
