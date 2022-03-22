import exec from "@actions/exec";
export const exec1Impl = exec.exec;
export const exec2Impl = exec.exec;

export function exec2Impl2(command, options) {
  return exec.exec(command, undefined, options);
}

export const exec3Impl = exec.exec;
