### What is an Action?

A [GitHub Action](https://github.com/features/actions) is a script which allows you to automate your GitHub workflows.

Specifically, an Action is an `action.yml` file paired with a Node script (found in `dist/index.js`). The `action.yml` file defines the inputs, outputs, and other metadata used by the Action, which the user can control in their own project. GitHub will execute your Node script with the parameters from the `action.yml` file after various events such as pushing to a branch or creating a pull request. Once you publish a GitHub Action, it can be used in other repositories to automate their workflows.

### What is GitHub's Actions Toolkit?

[GitHub's Actions Toolkit](https://github.com/actions/toolkit) is a set of Node modules which provide useful tools for creating Actions. For example:

`GitHub.Actions.Core` provides bindings to the [core](https://github.com/actions/toolkit/tree/main/packages/core) package, which allows you to 
