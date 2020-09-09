# GitHub.Actions.Exec

This module exposes bindings to the [@actions/exec package](https://github.com/actions/toolkit/tree/main/packages/exec).

## Usage

#### Basic

You can use this package to execute tools in a cross platform way:

```purescript
Exec.exec' "node index.js"
```

#### Args

You can also pass in arg arrays:

```purescript
Exec.exec { command: "node", args: Just [ "index.js", "foo=bar" ], options: Nothing }
```

#### Output/options

Capture output or specify other options:

```purescript
Exec.exec { command: "node", args: Just [ "index.js", "foo=bar" ], options: Just (Exec.defaultExecOptions { silent = Just true }) }
```

#### Exec tools not in the PATH

You can specify the full path for tools not in the PATH:

```purescript
Exec.exec { command: "/path/to/my-tool", args: Just [ "arg1"], options: Nothing }
```
