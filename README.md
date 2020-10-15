# GitHub Actions Toolkit

[![CI](https://github.com/purescript-contrib/purescript-github-actions-toolkit/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-github-actions-toolkit/actions?query=workflow%3ACI+branch%3Amain)
[![Release](http://img.shields.io/github/release/purescript-contrib/purescript-github-actions-toolkit.svg)](https://github.com/purescript-contrib/purescript-github-actions-toolkit/releases)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-github-actions-toolkit/badge)](http://pursuit.purescript.org/packages/purescript-github-actions-toolkit)
[![Maintainer: colinwahl](https://img.shields.io/badge/maintainer-colinwahl-teal.svg)](http://github.com/colinwahl)

This library provides PureScript bindings to Github's [Actions Toolkit](https://github.com/actions/toolkit), allowing you to automate your workflows with [Actions](https://github.com/features/actions) written in PureScript.

## Installation

Install GitHub Actions Toolkit with [Spago](https://github.com/purescript/spago):

```sh
spago install github-actions-toolkit
```

You will also need to install the npm packages for any bindings that you are using. For example, if you use functions exported from `GitHub.Actions.Core`, then you need to install the `@actions/core` npm package:

```sh
npm install @actions/core
```

## Quick start

An Action is an `action.yml` file pair with a Node script.

This library provides PureScript bindings to [Github's Actions Toolkit](https://github.com/actions/toolkit), which provides useful tools for writing Actions.

To write your own Action, create an `action.yml` file which specifies the inputs, outputs, and metadata which will be used by your Action. See [GitHub's docs](https://docs.github.com/en/actions/creating-actions/metadata-syntax-for-github-actions) on the syntax of this file. Then you can use this library to write a Node script which will execute the Action based on the `action.yml` file.

You can use the [Hello World PureScript Action template](https://github.com/colinwahl/hello-world-purescript-action) to get started defining your own Action. The template provides a starting point in order to define your own actions with these bindings!

Here are some common functions which are used when defining Actions:

Get an input with key `username` specified by the `action.yml` file for use in the script, then log it:

```purescript
main :: Effect Unit
main = void $ runExceptT do
  username <- Core.getInput { name: "username", options: Just { required: true }}
  liftEffect $ Core.info username
```

Use `which` to check that a tool is installed, and set the job to failed if it isn't.

```purescript
main :: Effect Unit
main = do
  result <- runExceptT (IO.which { tool: "spago", check: true })
  case result of
    Left err ->
      Core.error "spago not found"
      Core.setFailed "Required tool spago is not available for this job."
    Right spagoPath ->
      Core.info $ "spago found at path " <> spagoPath
      pure unit -- If your job ends without failing, then it succeeded.
```

Run an arbitrary command with `exec`.

```purescript
main :: Effect Unit
main = do
  result <- runExceptT (Exec.exec' "spago build")
  case result of
    Left err ->
      -- Something bad happened, log error and set failed
      Core.error $ message err
      Core.setFailed "Exception was thrown during spago build"
    Right returnCode | returnCode == 0.0 ->
      Core.info "spago build succeeded"
    Right returnCode ->
      Core.warning $ "spago build failed with ruturn code" <> returnCode
      Core.setFailed "spago exited with nonzero return code"
```

You can find documentation for all of the functions in this library in [the docs directory](./docs).

## Documentation

GitHub Actions Toolkit documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-github-actions-toolkit).
2. Written documentation is in [the docs directory](./docs).

For a usage example, see the [Hello World PureScript Action template](https://github.com/colinwahl/hello-world-purescript-action).  For a real-world action that uses these bindings, see [Setup PureScript](https://github.com/purescript-contrib/setup-purescript).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-github-actions-toolkit/issues) if you have encountered a bug or problem.
- [Search or start a thread on the PureScript Discourse](https://discourse.purescript.org) if you have general questions. You can also ask questions in the `#purescript` and `#purescript-beginners` channels on the [Functional Programming Slack](https://functionalprogramming.slack.com) ([invite link](https://fpchat-invite.herokuapp.com/)).

## Contributing

You can contribute to GitHub Actions Toolkit in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-github-actions-toolkit/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
