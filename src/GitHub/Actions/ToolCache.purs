-- | Exports functions from the @actions/tool-cache module provided by GitHub
-- | https://github.com/actions/toolkit/tree/main/packages/tool-cache
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

import Control.Monad.Error.Class (try)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.MonadPlus (guard)
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Tuple.Nested ((/\))
import Data.Version (Version)
import Data.Version as Version
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5)
import GitHub.Actions.OptionalArguments (Optional2, handleOptional2)
import GitHub.Actions.Types (Tool)
import GitHub.Actions.Utils (tryActionsM)
import Node.Path (FilePath)

foreign import downloadTool1Impl :: EffectFn1 String (Promise FilePath)

foreign import downloadTool2Impl :: EffectFn2 String FilePath (Promise FilePath)

foreign import downloadTool3Impl :: EffectFn3 String FilePath String (Promise FilePath)

-- | Download a tool from an url and stream it into a file
downloadTool
  :: String
  -> Optional2 FilePath String
  -> ExceptT Error Aff FilePath
downloadTool url = do
  handleOptional
    >>> toAffE
    >>> tryActionsM
  where
  handleOptional = handleOptional2
    { none: \_ -> runEffectFn1 downloadTool1Impl url
    , one: \dest -> runEffectFn2 downloadTool2Impl url dest
    , two: \(dest /\ auth) -> runEffectFn3 downloadTool3Impl url dest auth
    }

foreign import extract7c1Impl :: EffectFn1 FilePath (Promise FilePath) -- { file :: FilePath, dest :: Nullable FilePath, _7zPath :: Nullable String } -> Effect (Promise (Nullable String))

foreign import extract7c2Impl :: EffectFn2 FilePath FilePath (Promise FilePath)

foreign import extract7c3Impl :: EffectFn3 FilePath FilePath FilePath (Promise FilePath)

-- | Extract a .7z file
extract7c :: FilePath -> Optional2 FilePath FilePath -> ExceptT Error Aff String
extract7c file =
  handleOptional
    >>> toAffE
    >>> tryActionsM
  where
  handleOptional = handleOptional2
    { none: \_ -> runEffectFn1 extract7c1Impl file
    , one: \dest -> runEffectFn2 extract7c2Impl file dest
    , two: \(dest /\ _7cPath) -> runEffectFn3 extract7c3Impl file dest _7cPath
    }

foreign import extractTar1Impl :: EffectFn1 FilePath (Promise FilePath)

foreign import extractTar2Impl :: EffectFn2 FilePath FilePath (Promise FilePath)

foreign import extractTar3Impl :: EffectFn3 FilePath FilePath (Array String) (Promise FilePath)

-- | Extract a compressed tar archive
extractTar :: FilePath -> Optional2 FilePath (Array String) -> ExceptT Error Aff FilePath
extractTar file =
  handleOptional
    >>> toAffE
    >>> tryActionsM
  where
  handleOptional = handleOptional2
    { none: \_ -> runEffectFn1 extractTar1Impl file
    , one: \dest -> runEffectFn2 extractTar2Impl file dest
    , two: \(dest /\ flags) -> runEffectFn3 extractTar3Impl file dest flags
    }

foreign import extractXar1Impl :: EffectFn1 FilePath (Promise FilePath)  --{ file :: FilePath, dest :: Nullable FilePath, flags :: Nullable String } -> Effect (Promise (Nullable FilePath))

foreign import extractXar2Impl :: EffectFn2 FilePath FilePath (Promise FilePath)

foreign import extractXar3Impl :: EffectFn3 FilePath FilePath (Array String) (Promise FilePath)

-- | Extract a xar compatible archive
extractXar :: FilePath -> Optional2 FilePath (Array String) -> ExceptT Error Aff FilePath
extractXar file =
  handleOptional
    >>> toAffE
    >>> tryActionsM
  where
  handleOptional = handleOptional2
    { none: \_ -> runEffectFn1 extractXar1Impl file
    , one: \dest -> runEffectFn2 extractXar2Impl file dest
    , two: \(dest /\ flags) -> runEffectFn3 extractXar3Impl file dest flags
    }

foreign import extractZip1Impl :: EffectFn1 FilePath (Promise FilePath)

foreign import extractZip2Impl :: EffectFn2 FilePath FilePath (Promise FilePath)

-- | Extract a zip
extractZip :: { file :: FilePath, dest :: Maybe FilePath } -> ExceptT Error Aff FilePath
extractZip { file, dest: mbDest } =
  handleOptions
    # toAffE
    # tryActionsM
  where
  handleOptions = case mbDest of
    Nothing -> runEffectFn1 extractZip1Impl file
    Just dest -> runEffectFn2 extractZip2Impl file dest

foreign import cacheDir3Impl :: EffectFn3 FilePath Tool String (Promise FilePath)

foreign import cacheDir4Impl :: EffectFn4 FilePath Tool String String (Promise FilePath)

-- | Caches a directory and installs it into the tool cacheDir
cacheDir :: { sourceDir :: String, tool :: Tool, version :: Version, arch :: Maybe String } -> ExceptT Error Aff FilePath
cacheDir { sourceDir, tool, version, arch: mbArch } =
  handleOptions
    # toAffE
    # tryActionsM
  where
  handleOptions = case mbArch of
    Nothing -> runEffectFn3 cacheDir3Impl sourceDir tool (Version.showVersion version)
    Just arch -> runEffectFn4 cacheDir4Impl sourceDir tool (Version.showVersion version) arch

foreign import cacheFile4Impl :: EffectFn4 FilePath FilePath Tool String (Promise FilePath)

foreign import cacheFile5Impl :: EffectFn5 FilePath FilePath Tool String String (Promise FilePath)

-- | Caches a downloaded file (GUID) and installs it
-- | into the tool cache with a given targetName
cacheFile :: { sourceFile :: FilePath, targetFile :: FilePath, tool :: Tool, version :: Version, arch :: Maybe String } -> ExceptT Error Aff FilePath
cacheFile { sourceFile, targetFile, tool, version, arch: mbArch } =
  handleOptions
    # toAffE
    # tryActionsM
  where
  handleOptions = case mbArch of
    Nothing -> runEffectFn4 cacheFile4Impl sourceFile targetFile tool (Version.showVersion version)
    Just arch -> runEffectFn5 cacheFile5Impl sourceFile targetFile tool (Version.showVersion version) arch

foreign import find2Impl :: EffectFn2 Tool String FilePath

foreign import find3Impl :: EffectFn3 Tool String String FilePath

-- | Finds the path to a tool version in the local installed tool cache
find :: { toolName :: Tool, versionSpec :: String, arch :: Maybe String } -> ExceptT Error Effect (Maybe FilePath)
find { toolName, versionSpec, arch: mbArch } =
  handleOptions
    # liftEffect
    # try >>> ExceptT
    # map (\path -> guard (path /= "") $> path)
  where
  handleOptions = case mbArch of
    Nothing -> runEffectFn2 find2Impl toolName versionSpec
    Just arch -> runEffectFn3 find3Impl toolName versionSpec arch

foreign import findAllVersions1Impl :: EffectFn1 Tool (Array FilePath)

foreign import findAllVersions2Impl :: EffectFn2 Tool String (Array FilePath)

-- | Finds the paths to all versions of a tool that are installed in the local tool cache
findAllVersions :: { toolName :: Tool, arch :: Maybe String } -> ExceptT Error Effect (Array FilePath)
findAllVersions { toolName, arch: mbArch } =
  handleOptions
    # liftEffect
    # try >>> ExceptT
  where
  handleOptions = case mbArch of
    Nothing -> runEffectFn1 findAllVersions1Impl toolName
    Just arch -> runEffectFn2 findAllVersions2Impl toolName arch

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

foreign import getManifestFromRepo2Impl :: EffectFn2 String String (Promise (Array JSIToolRelease))

foreign import getManifestFromRepo3Impl :: EffectFn3 String String String (Promise (Array JSIToolRelease))

foreign import getManifestFromRepo4Impl :: EffectFn4 String String String String (Promise (Array JSIToolRelease))

getManifestFromRepo :: { owner :: String, repo :: String } -> Optional2 String String -> ExceptT Error Aff (Array IToolRelease)
getManifestFromRepo { owner, repo } =
  handleOptions
    >>> toAffE
    >>> tryActionsM
    >>> map (map toIToolRelease)
  where
  handleOptions = handleOptional2
    { none: \_ -> runEffectFn2 getManifestFromRepo2Impl owner repo
    , one: \auth -> runEffectFn3 getManifestFromRepo3Impl owner repo auth
    , two: \(auth /\ branch) -> runEffectFn4 getManifestFromRepo4Impl owner repo auth branch
    }

foreign import findFromManifest3Impl :: EffectFn3 String Boolean (Array JSIToolRelease) (Promise (Nullable JSIToolRelease))

foreign import findFromManifest4Impl :: EffectFn4 String Boolean (Array JSIToolRelease) String (Promise (Nullable JSIToolRelease))

findFromManifest :: { versionSpec :: String, stable :: Boolean, manifest :: Array IToolRelease, archFilter :: Maybe String } -> ExceptT Error Aff (Maybe IToolRelease)
findFromManifest { versionSpec, stable, manifest, archFilter: mbArchFilter } =
  handleOptions
    # toAffE
    # tryActionsM
    # map (toMaybe >>> map toIToolRelease)
  where
  handleOptions = case mbArchFilter of
    Nothing -> runEffectFn3 findFromManifest3Impl versionSpec stable (map fromIToolRelease manifest)
    Just archFilter -> runEffectFn4 findFromManifest4Impl versionSpec stable (map fromIToolRelease manifest) archFilter
