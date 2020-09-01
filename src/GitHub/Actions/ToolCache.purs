-- | Exports functions from the @actions/tool-cache module provided by GitHub
-- | https://github.com/actions/toolkit/tree/main/packages/tool-cache
module GitHub.Actions.ToolCache
  ( downloadTool
  , downloadTool'
  , DownloadToolArgs
  , extract7z
  , extract7z'
  , Extract7zArgs
  , extractTar
  , extractTar'
  , ExtractTarArgs
  , extractXar
  , extractXar'
  , ExtractXarArgs
  , extractZip
  , extractZip'
  , ExtractZipArgs
  , cacheDir
  , cacheDir'
  , CacheDirArgs
  , cacheFile
  , cacheFile'
  , CacheFileArgs
  , find
  , find'
  , FindArgs
  , findAllVersions
  , findAllVersions'
  , FindAllVersionsArgs
  , getManifestFromRepo
  , getManifestFromRepo'
  , GetManifestFromRepoArgs
  , findFromManifest
  , FindFromManifestArgs
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
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5)
import Node.Path (FilePath)

foreign import downloadTool1Impl :: EffectFn1 String (Promise FilePath)

foreign import downloadTool2Impl :: EffectFn2 String FilePath (Promise FilePath)

foreign import downloadTool2Impl2 :: EffectFn2 String String (Promise FilePath)

foreign import downloadTool3Impl :: EffectFn3 String FilePath String (Promise FilePath)

-- | url: url of tool to download
-- | dest: path to download tool. Defaults to randomly generated path
-- | auth: authorization header
type DownloadToolArgs =
  { url :: String
  , dest :: Maybe FilePath
  , auth :: Maybe String
  }

-- | Download a tool from an url and stream it into a file
downloadTool' :: String -> ExceptT Error Aff FilePath
downloadTool' url = downloadTool { url, dest: Nothing, auth: Nothing }

-- | Download a tool from an url and stream it into a file
downloadTool :: DownloadToolArgs -> ExceptT Error Aff FilePath
downloadTool =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
  where
  handleOptions { url, dest, auth } = case dest, auth of
    Nothing, Nothing -> runEffectFn1 downloadTool1Impl url
    Just d, Nothing -> runEffectFn2 downloadTool2Impl url d
    Nothing, Just a -> runEffectFn2 downloadTool2Impl url a
    Just d, Just a -> runEffectFn3 downloadTool3Impl url d a

foreign import extract7z1Impl :: EffectFn1 FilePath (Promise FilePath)

foreign import extract7z2Impl :: EffectFn2 FilePath FilePath (Promise FilePath)

foreign import extract7z2Impl2 :: EffectFn2 FilePath FilePath (Promise FilePath)

foreign import extract7z3Impl :: EffectFn3 FilePath FilePath FilePath (Promise FilePath)

-- | file: path to the .7z file
-- | dest: destination directory. Optional.
-- | _7zPath: path to 7zr.exe. Optional, for long path support.
type Extract7zArgs =
  { file :: FilePath
  , dest :: Maybe FilePath
  , _7zPath :: Maybe FilePath
  }

-- | Extract a .7z file. Returns path to the destination directory
extract7z' :: FilePath -> ExceptT Error Aff FilePath
extract7z' file = extract7z { file, dest: Nothing, _7zPath: Nothing }

-- | Extract a .7z file. Returns path to the destination directory
extract7z :: Extract7zArgs -> ExceptT Error Aff FilePath
extract7z =
  handleOptional
    >>> toAffE
    >>> (try >>> ExceptT)
  where
  handleOptional { file, dest, _7zPath } = case dest, _7zPath of
    Nothing, Nothing -> runEffectFn1 extract7z1Impl file
    Just d, Nothing -> runEffectFn2 extract7z2Impl file d
    Nothing, Just _7z -> runEffectFn2 extract7z2Impl2 file _7z
    Just d, Just _7z -> runEffectFn3 extract7z3Impl file d _7z

foreign import extractTar1Impl :: EffectFn1 FilePath (Promise FilePath)

foreign import extractTar2Impl :: EffectFn2 FilePath FilePath (Promise FilePath)

foreign import extractTar2Impl2 :: EffectFn2 FilePath (Array String) (Promise FilePath)

foreign import extractTar3Impl :: EffectFn3 FilePath FilePath (Array String) (Promise FilePath)

-- | file: path to the tar
-- | dest: destination directory. Defaults to randomly generated path
-- | flags: flags for the tar command to use for extraction. Defaults to 'xz' (extracting gzipped tars).
type ExtractTarArgs =
  { file :: FilePath
  , dest :: Maybe FilePath
  , flags :: Maybe (Array String)
  }

-- | Extract a compressed tar archive. Returns path to the destination directory
extractTar' :: FilePath -> ExceptT Error Aff FilePath
extractTar' file = extractTar { file, dest: Nothing, flags: Nothing }

-- | Extract a compressed tar archive. Returns path to the destination directory
extractTar :: ExtractTarArgs -> ExceptT Error Aff FilePath
extractTar =
  handleOptional
    >>> toAffE
    >>> (try >>> ExceptT)
  where
  handleOptional { file, dest, flags } = case dest, flags of
    Nothing, Nothing -> runEffectFn1 extractTar1Impl file
    Just d, Nothing -> runEffectFn2 extractTar2Impl file d
    Nothing, Just f -> runEffectFn2 extractTar2Impl2 file f
    Just d, Just f -> runEffectFn3 extractTar3Impl file d f

foreign import extractXar1Impl :: EffectFn1 FilePath (Promise FilePath)

foreign import extractXar2Impl :: EffectFn2 FilePath FilePath (Promise FilePath)

foreign import extractXar2Impl2 :: EffectFn2 FilePath (Array String) (Promise FilePath)

foreign import extractXar3Impl :: EffectFn3 FilePath FilePath (Array String) (Promise FilePath)

-- | file: path to the archive
-- | dest: destination directory. Defaults to randomly generated path
-- | flags: flags for the xar
type ExtractXarArgs =
  { file :: FilePath
  , dest :: Maybe FilePath
  , flags :: Maybe (Array String)
  }

-- | Extract a xar compatible archive. Returns path to the destination directory
extractXar' :: FilePath -> ExceptT Error Aff FilePath
extractXar' file = extractXar { file, dest: Nothing, flags: Nothing }

-- | Extract a xar compatible archive. Returns path to the destination directory
extractXar :: ExtractXarArgs -> ExceptT Error Aff FilePath
extractXar =
  handleOptional
    >>> toAffE
    >>> (try >>> ExceptT)
  where
  handleOptional { file, dest, flags } = case dest, flags of
    Nothing, Nothing -> runEffectFn1 extractXar1Impl file
    Just d, Nothing -> runEffectFn2 extractXar2Impl file d
    Nothing, Just f -> runEffectFn2 extractXar2Impl2 file f
    Just d, Just f ->  runEffectFn3 extractXar3Impl file d f

foreign import extractZip1Impl :: EffectFn1 FilePath (Promise FilePath)

foreign import extractZip2Impl :: EffectFn2 FilePath FilePath (Promise FilePath)

-- file: path to the zip
-- dest: destination directory. Defaults to randomly generated path
type ExtractZipArgs =
  { file :: FilePath
  , dest :: Maybe FilePath
  }

-- | Extract a zip. Returns path to the destination directory
extractZip' :: FilePath -> ExceptT Error Aff FilePath
extractZip' file = extractZip { file, dest: Nothing }

-- | Extract a zip. Returns path to the destination directory
extractZip :: ExtractZipArgs -> ExceptT Error Aff FilePath
extractZip =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
  where
  handleOptions { file, dest } = case dest of
    Nothing -> runEffectFn1 extractZip1Impl file
    Just d -> runEffectFn2 extractZip2Impl file d

foreign import cacheDir3Impl :: EffectFn3 FilePath String String (Promise FilePath)

foreign import cacheDir4Impl :: EffectFn4 FilePath String String String (Promise FilePath)

-- | sourceDir: the directory to cache into tools
-- | tool: tool name
-- | version: version of the tool.  semver format
-- | architecture of the tool. Defaults to machine architecture
type CacheDirArgs =
  { sourceDir :: String
  , tool :: String
  , version :: String
  , arch :: Maybe String
  }

-- | Caches a directory and installs it into the tool cacheDir
cacheDir' :: { sourceDir :: String, tool :: String , version :: String } -> ExceptT Error Aff FilePath
cacheDir' { sourceDir, tool, version } = cacheDir { sourceDir, tool, version, arch: Nothing }

-- | Caches a directory and installs it into the tool cacheDir
cacheDir :: CacheDirArgs -> ExceptT Error Aff FilePath
cacheDir =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
  where
  handleOptions { sourceDir, tool, version, arch } = case arch of
    Nothing -> runEffectFn3 cacheDir3Impl sourceDir tool version
    Just a -> runEffectFn4 cacheDir4Impl sourceDir tool version a

foreign import cacheFile4Impl :: EffectFn4 FilePath FilePath String String (Promise FilePath)

foreign import cacheFile5Impl :: EffectFn5 FilePath FilePath String String String (Promise FilePath)

-- | sourceFile: the file to cache into tools.  Typically a result of downloadTool which is a guid
-- | targetFile: the name of the file name in the tools directory
-- | tool: tool name
-- | version: version of the tool.  semver format
-- | arch: architecture of the tool. Defaults to machine architecture
type CacheFileArgs =
  { sourceFile :: FilePath
  , targetFile :: FilePath
  , tool :: String
  , version :: String
  , arch :: Maybe String
  }

-- | Caches a downloaded file (GUID) and installs it into the tool cache with a given targetName
cacheFile' :: { sourceFile :: FilePath, targetFile :: FilePath, tool :: String, version :: String } -> ExceptT Error Aff FilePath
cacheFile' { sourceFile, targetFile, tool, version } = cacheFile { sourceFile, targetFile, tool, version, arch: Nothing }

-- | Caches a downloaded file (GUID) and installs it into the tool cache with a given targetName
cacheFile :: CacheFileArgs -> ExceptT Error Aff FilePath
cacheFile =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
  where
  handleOptions { sourceFile, targetFile, tool, version, arch } = case arch of
    Nothing -> runEffectFn4 cacheFile4Impl sourceFile targetFile tool version
    Just a -> runEffectFn5 cacheFile5Impl sourceFile targetFile tool version a

foreign import find2Impl :: EffectFn2 String String FilePath

foreign import find3Impl :: EffectFn3 String String String FilePath

-- | toolName: name of the tool
-- | version: version of the tool.
-- | arch: architecture of the tool. Defaults to machine architecture
type FindArgs =
  { toolName :: String
  , versionSpec :: String
  , arch :: Maybe String
  }

-- | Finds the path to a tool version in the local installed tool cache
find' :: { toolName :: String, versionSpec :: String } -> ExceptT Error Effect (Maybe FilePath)
find' { toolName, versionSpec } = find { toolName, versionSpec, arch: Nothing }

-- | Finds the path to a tool version in the local installed tool cache
find :: FindArgs -> ExceptT Error Effect (Maybe FilePath)
find =
  handleOptions
    >>> liftEffect
    >>> (try >>> ExceptT)
    >>> map (\path -> guard (path /= "") $> path)
  where
  handleOptions { toolName, versionSpec, arch } = case arch of
    Nothing -> runEffectFn2 find2Impl toolName versionSpec
    Just a -> runEffectFn3 find3Impl toolName versionSpec a

foreign import findAllVersions1Impl :: EffectFn1 String (Array FilePath)

foreign import findAllVersions2Impl :: EffectFn2 String String (Array FilePath)

-- | toolName: name of the tool
-- | arch: architecture of the tool. Defaults to machine architecture
type FindAllVersionsArgs =
  { toolName :: String
  , arch :: Maybe String
  }

-- | Finds the paths to all versions of a tool that are installed in the local tool cache
findAllVersions' :: String -> ExceptT Error Effect (Array FilePath)
findAllVersions' toolName = findAllVersions { toolName, arch: Nothing }

-- | Finds the paths to all versions of a tool that are installed in the local tool cache
findAllVersions :: FindAllVersionsArgs -> ExceptT Error Effect (Array FilePath)
findAllVersions =
  handleOptions
    >>> liftEffect
    >>> (try >>> ExceptT)
  where
  handleOptions { toolName, arch } = case arch of
    Nothing -> runEffectFn1 findAllVersions1Impl toolName
    Just a -> runEffectFn2 findAllVersions2Impl toolName a

-- | Details for a release file
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

-- | Details for a release
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

foreign import getManifestFromRepo3Impl2 :: EffectFn3 String String String (Promise (Array JSIToolRelease))

foreign import getManifestFromRepo4Impl :: EffectFn4 String String String String (Promise (Array JSIToolRelease))

-- | owner: repository owner
-- | repo: repository name
-- | auth: auth token
-- | branch: branch of the repository. Defaults to 'master'
type GetManifestFromRepoArgs =
  { owner :: String
  , repo :: String
  , auth :: Maybe String
  , branch :: Maybe String
  }

-- Get list of releases from a repository
getManifestFromRepo' :: { owner :: String, repo :: String } -> ExceptT Error Aff (Array IToolRelease)
getManifestFromRepo' { owner, repo } = getManifestFromRepo { owner, repo, auth: Nothing, branch: Nothing }

-- Get list of releases from a repository
getManifestFromRepo :: GetManifestFromRepoArgs -> ExceptT Error Aff (Array IToolRelease)
getManifestFromRepo =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
    >>> map (map toIToolRelease)
  where
  handleOptions { owner, repo, auth, branch } = case auth, branch of
    Nothing, Nothing -> runEffectFn2 getManifestFromRepo2Impl owner repo
    Just a, Nothing -> runEffectFn3 getManifestFromRepo3Impl owner repo a
    Nothing, Just b -> runEffectFn3 getManifestFromRepo3Impl2 owner repo b
    Just a, Just b -> runEffectFn4 getManifestFromRepo4Impl owner repo a b

foreign import findFromManifest3Impl :: EffectFn3 String Boolean (Array JSIToolRelease) (Promise (Nullable JSIToolRelease))

foreign import findFromManifest4Impl :: EffectFn4 String Boolean (Array JSIToolRelease) String (Promise (Nullable JSIToolRelease))

-- versionSpec: version to search for
-- stable: whether or not the release is stable
-- manifest: manifests to search
-- archFilter: architecture filter. Defaults to machine architecture
type FindFromManifestArgs =
  { versionSpec :: String
  , stable :: Boolean
  , manifest :: Array IToolRelease
  , archFilter :: Maybe String
  }

-- Search list of releases from a repository
findFromManifest :: FindFromManifestArgs -> ExceptT Error Aff (Maybe IToolRelease)
findFromManifest =
  handleOptions
    >>> toAffE
    >>> (try >>> ExceptT)
    >>> map (toMaybe >>> map toIToolRelease)
  where
  handleOptions { versionSpec, stable, manifest, archFilter } = case archFilter of
    Nothing -> runEffectFn3 findFromManifest3Impl versionSpec stable (map fromIToolRelease manifest)
    Just a -> runEffectFn4 findFromManifest4Impl versionSpec stable (map fromIToolRelease manifest) a
