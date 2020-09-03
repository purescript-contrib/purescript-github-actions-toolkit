var exec = require("@actions/exec");

exports.exec1Impl = exec.exec;

exports.exec2Impl = exec.exec;

exports.exec2Impl2 = (command, options) =>
  exec.exec(command, undefined, options);

exports.exec3Impl = exec.exec;
