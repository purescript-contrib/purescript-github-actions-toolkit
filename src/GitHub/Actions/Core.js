"use strict";
const core = require("@actions/core");

exports.exportVariableImpl = core.exportVariable;

exports.setSecretImpl = core.setSecret;

exports.addPathImpl = core.addPath;

exports.getInput1Impl = core.getInput;

exports.getInput2Impl = core.getInput;

exports.setOutputImpl = core.setOutput;

exports.setCommandEchoImpl = core.setCommandEcho;

exports.setFailedImpl = core.setFailed;

exports.isDebugImpl = core.isDebug;

exports.debugImpl = core.debug;

exports.errorImpl = core.error;

exports.warningImpl = core.warning;

exports.infoImpl = core.info;

exports.startGroupImpl = core.startGroup;

exports.endGroupImpl = core.endGroup;

exports.saveStateImpl = core.saveState;

exports.getStateImpl = core.getState;

exports.groupImpl = core.group;
