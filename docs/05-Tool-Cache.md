# GitHub.Actions.ToolCache

> Functions necessary for downloading and caching tools.

This module exposes bindings to the [@actions/tool-cache package](https://github.com/actions/toolkit/tree/main/packages/tool-cache).

## Usage

#### Download

You can use this to download tools (or other files) from a download URL:

```purescript

node12Path <- ToolCache.downloadTool "https://nodejs.org/dist/v12.7.0/node-v12.7.0-linux-x64.tar.gz"
```

#### Extract

These can then be extracted in platform specific ways:

```purescript
const tc = require('@actions/tool-cache');

node12ExtractedFolder <- case platform of
  Just Win32 -> do
    node12Path <- ToolCache.downloadTool' "https://nodejs.org/dist/v12.7.0/node-v12.7.0-win-x64.zip"
    ToolCache.extractZip' node12Path
  Just Darwin -> do
    node12Path <- ToolCache.downloadTool' "https://nodejs.org/dist/v12.7.0/node-v12.7.0.pkg"
    ToolCache.extractXar' node12Path
  _ -> do
    node12Path <- ToolCache.downloadTool' "https://nodejs.org/dist/v12.7.0/node-v12.7.0-linux-x64.tar.gz"
    ToolCache.extractTar node12Path
```

#### Cache

Finally, you can cache these directories in our tool-cache. This is useful if you want to switch back and forth between versions of a tool, or save a tool between runs for self-hosted runners.

You'll often want to add it to the path as part of this step:

```purescript
const tc = require('@actions/tool-cache');
const core = require('@actions/core');

example = do
  node12Path <- ToolCache.downloadTool' "https://nodejs.org/dist/v12.7.0/node-v12.7.0-linux-x64.tar.gz"
  node12ExtractedFolder <- ToolCache.extractTar node12Path
  cachedPath <- ToolCache.cacheDir' { sourceDir: node12ExtractedFolder, tool: "node", version: "12.7.0" }
  liftEffect $ do
    Core.addPath cachedPath
```

You can also cache files for reuse.

```purescript
ToolTache.cachefile' { sourceFile: "path/to/exe", targetFile: "destFileName.exe", tool: "myExeName", version: "1.1.0"}
```

#### Find

Finally, you can find directories and files you've previously cached:

```purescript
nodeDirectory <- ToolCache.find' { toolName: "node", versionSpec "12.x" }
liftEffect $ Core.addPath nodeDirectory
```

You can even find all cached versions of a tool:

```purescript
allNodeVersion <- ToolCache.findAllVersions "node"
```
