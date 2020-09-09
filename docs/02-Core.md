# GitHub.Actions.Core

> Core functions for setting results, logging, registering secrets and exporting variables across actions

This module exposes bindings to the [@actions/core package](https://github.com/actions/toolkit/tree/main/packages/core).

## Usage

#### Inputs/Outputs

Action inputs can be read with `getInput`.  Outputs can be set with `setOutput` which makes them available to be mapped into inputs of other actions to ensure they are decoupled.

```purescript
void $ runExceptT do
  myInput <- Core.getInput' "inputName"
  liftEffect $ Core.setOutput "outputKey" "outputVal"
```

#### Exporting variables

Since each step runs in a separate process, you can use `exportVariable` to add it to this step and future steps environment blocks.

```purescript
  Core.exportVariable { key: "envVar", val: "Val"}
```

#### Setting a secret

Setting a secret registers the secret with the runner to ensure it is masked in logs.

```purescript
example = do
  Core.setSecret "myPassword"
```

#### PATH Manipulation

To make a tool's path available in the path for the remainder of the job (without altering the machine or containers state), use `addPath`.  The runner will prepend the path given to the jobs PATH.

```purescript
example = do
  Core.addPath "/path/to/mytool"
```

#### Exit codes

You should use this library to set the failing exit code for your action.  If status is not set and the script runs to completion, that will lead to a success.

```purescript
task = mempty -- do stuff that might fail here

example = do
  case runExceptT task of
    Left err -> Core.setFailed $ "Action failed with error " <> message err
    Right _ -> mempty -- task succeded
```

#### Logging

Finally, this library provides some utilities for logging. Note that debug logging is hidden from the logs by default. This behavior can be toggled.

```js
task = do
  myInput <- Core.getInput' "input"
  Core.debug ""
  core.debug('Inside try block');
  isDebug <- Core.isDebug
  liftEffect $ when isDebug do
    -- handle debug messages here
  Core.info "output to actions build log"
  
  
example = do
  case runExceptT task of
    Left err -> Core.error $ "task failed with error " <> message err -- Note: action may still succeed
    Right _ -> mempty
```

This library can also wrap chunks of output in foldable groups.

```js
example = do
  -- manually wrap output
  Core.startGroup "do some function"
  doSomeFunction
  Core.endGroup
  
  -- wrap an asynchronous function call
  result <- Core.group 
    { name: "do something async"
    , fn: do
        -- get the result from something asynchronous in the Aff monad
        result <- taskInAff
        pure result
    }
```

#### Action state

You can use this library to save state and get state for sharing information between a given wrapper action: 

**action.yml**
```yaml
name: 'Wrapper action sample'
inputs:
  name:
    default: 'GitHub'
runs:
  using: 'node12'
  main: 'main.js'
  post: 'cleanup.js'
```

In action's `main.js`:

```js
Core.saveState { name: "pidToKill", value: "12345" }
```

In action's `cleanup.js`:
```js
pid <- Core.getState "pidToKill" 
-- Do something with pid
```
