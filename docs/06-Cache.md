# GitHub.Actions.Cache

> Functions necessary for caching dependencies and build outputs to improve workflow execution time.

This module exposes bindings to the [@actions/cache package](https://github.com/actions/toolkit/tree/main/packages/cache). See [the @actions/cache README](https://github.com/actions/toolkit/tree/main/packages/cache#actionscache) for more details and important cache limit information.

## Usage

This package is used by the v2+ versions of our first party cache action. You can find an example implementation in the cache repo [here](https://github.com/actions/cache). 

#### Restore Cache

Restores a cache based on `key` and `restoreKeys` to the `paths` provided. Function returns the cache key for cache hit and returns undefined if cache not found. 

```purescript
let paths =
  [ "node_modules"
  , "packages/*/node_modules"
  ]
let primaryKey = 'npm-foobar-d5ea0750'
let restoreKeys =
  [ "npm-foobar-"
  , "npm-"
  ]
Cache.restoreCache { paths, primaryKey, restoreKeys: Just restoreKeys, options: Nothing }
```

#### Save Cache

Saves a cache containing the files in `paths` using the `key` provided. The files would be compressed using zstandard compression algorithm if zstd is installed, otherwise gzip is used. Function returns the cache id if the cache was saved succesfully and throws an error if cache upload fails. 

```purescript
let paths =
  [ "node_modules"
  , "packages/*/node_modules/"
  ]
let key = 'npm-foobar-d5ea0750'
cacheId <- Cache.saveCache { paths, key }
```

