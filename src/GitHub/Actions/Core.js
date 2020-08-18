const core = require("@actions/core");

exports.exportVariable = ({ key, value }) => () =>
  core.exportVariable(key, value);

exports.setSecret = (secret) => () =>
  core.setSecret(secret);

exports.addPath = (path) => () =>
  core.addPath(path);

exports.getInputImpl = ({ name, options }) => () =>
  core.getInput(name, options);

exports.setOutput = ({ name, value }) => () =>
  core.setOutput(name, value);

exports.setCommandEcho = (enabled) => () =>
  core.setCommandEcho(enabled);

exports.setFailed = (msg) => () =>
  core.setFailed(msg);

exports.isDebug = () =>
  core.isDebug();

exports.debug = (msg) => () =>
  core.debug(msg);

exports.error = (msg) => () =>
  core.error(msg);

exports.warning = (msg) => () =>
  core.warning(msg);

exports.info = (msg) => () =>
  core.info(msg);

exports.startGroup = (name) => () =>
  core.startGroup(name);

exports.endGroup = (name) => () =>
  core.endGroup(name);

exports.saveState = ({ name, value }) => () =>
  core.saveState(name, value);

exports.getStateImpl = (name) => () =>
  core.getState(name);

exports.groupImpl = ({ name, fn }) => () =>
  core.group(name, fn);
