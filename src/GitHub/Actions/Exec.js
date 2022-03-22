"use strict";
import exec from "@actions/exec";
export var exec1Impl = exec.exec;
export var exec2Impl = exec.exec;

export function exec2Impl2(command, options) {
  return exec.exec(command, undefined, options);
}

export var exec3Impl = exec.exec;
