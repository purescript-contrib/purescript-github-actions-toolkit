module Test.Main where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import GitHub.Actions.OptionalArguments (class Optional1', Optional1'', requiredWithOne)
import GitHub.Actions.ToolCache as ToolCache
import Node.Path (FilePath)

extractZipArgs
  :: Optional1' ( file :: String ) "dest" String ( file :: String, dest :: String )
  => Optional1'' ( file :: String ) "dest" String ( file :: String, dest :: String )
extractZipArgs = requiredWithOne { file: "/path/to/file", dest: "/dest/path/to/file" }

extractZip :: ExceptT Error Aff FilePath
extractZip = ToolCache.extractZip extractZipArgs

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
