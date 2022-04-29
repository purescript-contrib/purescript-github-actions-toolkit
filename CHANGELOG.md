# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v0.5.0](https://github.com/purescript-contrib/purescript-github-actions-toolkit/releases/tag/v0.5.0) - 2022-04-29

Breaking changes:
- Migrate FFI to ES modules (#17 and #18 by @JordanMartinez)

## [v0.4.0](https://github.com/purescript-contrib/purescript-github-actions-toolkit/releases/tag/v0.4.0) - 2022-04-27

Due to an incorrectly-made breaking change, please use v0.5.0 instead.

## [v0.3.0](https://github.com/purescript-contrib/purescript-github-actions-toolkit/releases/tag/v0.3.0) - 2021-04-18

Breaking changes:

- Added support for PureScript 0.14 and dropped support for other compiler versions. (#16 by @thomashoneyman)
- Updated GitHub `@actions/*` dependencies for post-October 2020 compatibility. (#13 by @jisantuc)

## [v0.2.2](https://github.com/purescript-contrib/purescript-github-actions-toolkit/releases/tag/v0.2.2) - 2021-02-03

Other improvements:

- Added tests for the majority of bindings in the library (#9 by @colinwahl)
- Updated the location of the changelog and contributing files (#11 by @maxdeviant)
- Bumped the versions of @actions/core, @actions/cache, and @actions/tool-cache

## [v0.2.0](https://github.com/purescript-contrib/purescript-github-actions-toolkit/releases/tag/v0.2.0) - 2020-09-22

Bugfixes:

- Fixed a typo on the FFI for the `find` function (#8 by @colinwahl)

Other improvements

- Added a test action (#8 by @colinwahl)
- Added CI for all push / pull requests (#6 by @colinwahl)

## [v0.1.0](https://github.com/purescript-contrib/purescript-github-actions-toolkit/releases/tag/v0.1.0) - 2020-09-09

Initial release of GitHub Actions Toolkit bindings. This features full bindings to the following packages:

- @actions/core (as GitHub.Actions.Core)
- @actions/exec (as GitHub.Actions.Exec)
- @actions/io (as GitHub.Actions.IO)
- @actions/tool-cache (as GitHub.Actions.ToolCache)
- @actions/cache (as GitHub.Actions.Cache)
