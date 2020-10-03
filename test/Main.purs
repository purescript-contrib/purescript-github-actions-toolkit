module Test.Main where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import GitHub.Actions.Cache as Cache
import GitHub.Actions.Core as Core
import GitHub.Actions.Exec as Exec
import GitHub.Actions.IO as IO
import GitHub.Actions.ToolCache as ToolCache

main :: Effect Unit
main = do
  Core.info "Starting test action..."

  -- Tests for GitHub.Actions.Core
  Core.exportVariable { key: "testkey", value: "test" }
  Core.setOutput { name: "testoutput", value: "test" }
  Core.setSecret "testoutput"
  Core.addPath "/test/path"
  resultCore <- runExceptT do
    _ <- Core.getInput' "testinput"
    _ <- Core.getInput { name: "testinput", options: Nothing }
    Core.getInput { name: "testinput", options: Just { required: true } }
  case resultCore of
    Left err -> Core.setFailed (message err)
    Right _ -> Core.info "No errors in GitHub.Actions.Core ExceptT functions"
  Core.setCommandEcho false
  _ <- Core.isDebug
  Core.debug "Testing debug"
  Core.error "Testing error"
  Core.warning "Testing warning"
  Core.info "Testing info"
  Core.startGroup "testgroup"
  Core.endGroup
  Core.saveState { name: "teststate", value: "test" }
  _ <- Core.getState "teststate"
  launchAff_ $ Core.group
    { name: "testGroup"
    , fn: liftEffect (Core.info "In testGroup")
    }

  -- Tests for GitHub.Actions.Cache
  let
    cacheCb = case _ of
      Left err -> Core.setFailed (message err)
      Right _ -> Core.info "No errors in restoreCache"
  runAff_ cacheCb $ runExceptT do
    _ <- Cache.saveCache' { paths: [], key: "restorecache" }
    _ <- Cache.saveCache { paths: [], key: "restorecache", options: Nothing }
    _ <- Cache.saveCache { paths: [], key: "restorecache", options: Just (Cache.defaultUploadOptions { uploadConcurrency = Just 10.0 }) }
    _ <- Cache.restoreCache' { paths: [], primaryKey: "restorecache" }
    _ <- Cache.restoreCache { paths: [], primaryKey: "restorecache", restoreKeys: Nothing, options: Nothing }
    _ <- Cache.restoreCache { paths: [], primaryKey: "restorecache", restoreKeys: Just [ "a" ], options: Nothing }
    Cache.restoreCache { paths: [], primaryKey: "restorecaceh", restoreKeys: Just [ "a" ], options: Just (Cache.defaultDownloadOptions { useAzureSdk = Just true, downloadConcurrency = Just true, timeoutInMs = Just 10.0 })}

  -- Tests for GitHub.Actions.Exec
  let
    execCb = case _ of
      Left err -> Core.setFailed (message err)
      Right _ -> Core.info "No errors in exec"
  runAff_ execCb $ runExceptT do
    _ <- Exec.exec' "ls"
    _ <- Exec.exec { command: "ls", args: Just [ "-a" ], options: Nothing }
    Exec.exec { command: "ls", args: Just [ "-a" ], options: Just (Exec.defaultExecOptions { delay = Just 10.0 })}

  -- Tests for GitHub.Actions.IO
  let
    ioCb = case _ of
      Left err -> Core.setFailed (message err)
      Right _ -> Core.info "No errors in io"
  runAff_ ioCb $ runExceptT do
    _ <- Exec.exec' "touch test.txt"
    _ <- IO.cp' { source: "test.txt", dest: "test1.txt" }
    _ <- IO.cp { source: "test1.txt", dest: "test2.txt", options: Just (IO.defaultCopyOptions { force = Just true })}
    _ <- IO.mv' { source: "test2.txt", dest: "test3.txt" }
    _ <- IO.mv { source: "test3.txt", dest: "test4.txt", options: Just (IO.defaultMoveOptions { force = Just true }) }
    _ <- IO.rmRF { inputPath: "test4.txt" }
    _ <- IO.mkdirP { fsPath: "testDir" }
    _ <- IO.which' "purs"
    IO.which { tool: "purs", check: Just true }

  -- Tests for GitHub.Actions.ToolCache
  let
    toolCacheCb = case _ of
      Left err -> Core.setFailed (message err)
      Right _ -> Core.info "No errors in ToolCache"
  runAff_ toolCacheCb $ runExceptT do
    pursPath <- ToolCache.downloadTool' "https://github.com/purescript/purescript/releases/download/v0.13.8/linux64.tar.gz"
    _ <- ToolCache.downloadTool { url: "https://github.com/purescript/purescript/releases/download/v0.13.8/linux64.tar.gz", dest: Just "~/purspath", auth: Nothing }
    _ <- ToolCache.downloadTool { url: "https://github.com/purescript/purescript/releases/download/v0.13.8/linux64.tar.gz", dest: Just "~/purspath", auth: Just "blahhh" }
    extractedPurs <- ToolCache.extractTar' pursPath
    _ <- ToolCache.extractTar { file: pursPath, dest: Just "/extractedpurs", flags: Nothing }
    _ <- ToolCache.extractTar { file: pursPath, dest: Just "/extractedpurs", flags: Just [] }
    zipPath <- ToolCache.downloadTool' "https://github.com/purescript/purescript/archive/v0.13.8.zip"
    _ <- ToolCache.extractZip' zipPath
    _ <- ToolCache.extractZip { file: zipPath, dest: Just "/extractedpurszip" }
    _ <- ToolCache.cacheDir' { sourceDir: extractedPurs, tool: "purs", version: "v0.13.8" }
    _ <- ToolCache.cacheDir { sourceDir: extractedPurs, tool: "purs", version: "v0.13.8", arch: Just "arm" }
    _ <- ToolCache.cacheFile' { sourceFile: pursPath, targetFile: "testtarget", tool: "purs", version: "v0.13.8" }
    _ <- ToolCache.cacheFile { sourceFile: pursPath, targetFile: "testtarget", tool: "purs", version: "v0.13.8", arch: Just "arm" }
    pursManifest <- ToolCache.getManifestFromRepo' { owner: "purescript", repo: "purescript" }
    _ <- ToolCache.getManifestFromRepo { owner: "purescript", repo: "purescript", auth: Just "blah", branch: Just "master" }
    _ <- ToolCache.findFromManifest { versionSpec: "v0.13.8", stable: true, manifest: pursManifest, archFilter: Nothing }
    ToolCache.findFromManifest { versionSpec: "v0.13.8", stable: true, manifest: pursManifest, archFilter: Just "arm" }

  result <- runExceptT do
    _ <- ToolCache.find' { toolName: "my-tool", versionSpec: "12.x" }
    _ <- ToolCache.find { toolName: "my-tool", versionSpec: "12.x", arch: Just "armv6" }
    _ <- ToolCache.findAllVersions' "purs"
    ToolCache.findAllVersions { toolName: "purs", arch: Just "arm" }
  case result of
    Left err -> Core.setFailed (message err)
    Right _ -> Core.info "No errors in find"
  Core.info "Done running test action."
