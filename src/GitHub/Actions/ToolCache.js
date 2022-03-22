"use strict";
import toolCache from "@actions/tool-cache";
export var downloadTool1Impl = toolCache.downloadTool;
export var downloadTool2Impl = toolCache.downloadTool;

export function downloadTool2Impl2(url, auth) {
  return toolCache.downloadTool(url, undefined, auth);
}

export var downloadTool3Impl = toolCache.downloadTool;
export var extract7z1Impl = toolCache.extract7z;
export var extract7z2Impl = toolCache.extract7z;

export function extract7z2Impl2(file, _7cPath) {
  return toolCache.extract7z(file, undefined, _7cPath);
}

export var extract7z3Impl = toolCache.extract7z;
export var extractTar1Impl = toolCache.extractTar;
export var extractTar2Impl = toolCache.extractTar;

export function extractTar2Impl2(file, flags) {
  return toolCache.extractTar(file, undefined, flags);
}

export var extractTar3Impl = toolCache.extractTar;
export var extractXar1Impl = toolCache.extractXar;
export var extractXar2Impl = toolCache.extractXar;

export function extractXar2Impl2(file, flags) {
  return toolCache.extractXar(file, undefined, flags);
}

export var extractXar3Impl = toolCache.extractXar;
export var extractZip1Impl = toolCache.extractZip;
export var extractZip2Impl = toolCache.extractZip;
export var cacheDir3Impl = toolCache.cacheDir;
export var cacheDir4Impl = toolCache.cacheDir;
export var cacheFile4Impl = toolCache.cacheFile;
export var cacheFile5Impl = toolCache.cacheFile;
export var find2Impl = toolCache.find;
export var find3Impl = toolCache.find;
export var findAllVersions1Impl = toolCache.findAllVersions;
export var findAllVersions2Impl = toolCache.findAllVersions;
export var getManifestFromRepo2Impl = toolCache.getManifestFromRepo;
export var getManifestFromRepo3Impl = toolCache.getManifestFromRepo;

export function getManifestFromRepo3Impl2(owner, repo, branch) {
  return toolCache.getManfiestFromRepo(owner, repo, undefined, branch);
}

export var getManifestFromRepo4Impl = toolCache.getManifestFromRepo;
export var findFromManifest3Impl = toolCache.findFromManifest;
export var findFromManifest4Impl = toolCache.findFromManifest;
