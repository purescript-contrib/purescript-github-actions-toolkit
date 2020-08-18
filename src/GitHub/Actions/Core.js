const core = require("@actions/core");

exports.exportVariableImpl = ({ key, value }) => () =>
  core.exportVariable(key, value);

exports.setSecretImpl = (secret) => () =>
  core.setSecret(secret);

exports.addPathImpl = (path) => () =>
  core.addPath(path);

exports.getInputImpl = ({ name, options }) => () =>
  core.getInput(name, options);

exports.setOutputImpl = ({ name, value }) => () =>
  core.setOutput(name, value);

exports.setCommandEchoImpl = (enabled) => () =>
  core.setCommandEcho(enabled);

exports.setFailedImpl = (msg) => () =>
  core.setFailed(msg);

exports.isDebugImpl = () =>
  core.isDebug();

exports.debugImpl = (msg) => () =>
  core.debug(msg);

exports.errorImpl = (msg) => () =>
  core.error(msg);

exports.warningImpl = (msg) => () =>
  core.warning(msg);

exports.infoImpl = (msg) => () =>
  core.info(msg);

exports.startGroupImpl = (name) => () =>
  core.startGroup(name);

exports.endGroupImpl = (name) => () =>
  core.endGroup(name);

exports.saveStateImpl = ({ name, value }) => () =>
  core.saveState(name, value);

exports.getStateImpl = (name) => () =>
  core.getState(name);

exports.groupImpl = ({ name, fn }) => () =>
  core.group(name, fn);
