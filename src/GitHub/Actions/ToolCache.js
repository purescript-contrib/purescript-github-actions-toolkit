import toolCache from "@actions/tool-cache";
export const downloadTool1Impl = toolCache.downloadTool;
export const downloadTool2Impl = toolCache.downloadTool;

export function downloadTool2Impl2(url, auth) {
  return toolCache.downloadTool(url, undefined, auth);
}

export const downloadTool3Impl = toolCache.downloadTool;
export const extract7z1Impl = toolCache.extract7z;
export const extract7z2Impl = toolCache.extract7z;

export function extract7z2Impl2(file, _7cPath) {
  return toolCache.extract7z(file, undefined, _7cPath);
}

export const extract7z3Impl = toolCache.extract7z;
export const extractTar1Impl = toolCache.extractTar;
export const extractTar2Impl = toolCache.extractTar;

export function extractTar2Impl2(file, flags) {
  return toolCache.extractTar(file, undefined, flags);
}

export const extractTar3Impl = toolCache.extractTar;
export const extractXar1Impl = toolCache.extractXar;
export const extractXar2Impl = toolCache.extractXar;

export function extractXar2Impl2(file, flags) {
  return toolCache.extractXar(file, undefined, flags);
}

export const extractXar3Impl = toolCache.extractXar;
export const extractZip1Impl = toolCache.extractZip;
export const extractZip2Impl = toolCache.extractZip;
export const cacheDir3Impl = toolCache.cacheDir;
export const cacheDir4Impl = toolCache.cacheDir;
export const cacheFile4Impl = toolCache.cacheFile;
export const cacheFile5Impl = toolCache.cacheFile;
export const find2Impl = toolCache.find;
export const find3Impl = toolCache.find;
export const findAllVersions1Impl = toolCache.findAllVersions;
export const findAllVersions2Impl = toolCache.findAllVersions;
export const getManifestFromRepo2Impl = toolCache.getManifestFromRepo;
export const getManifestFromRepo3Impl = toolCache.getManifestFromRepo;

export function getManifestFromRepo3Impl2(owner, repo, branch) {
  return toolCache.getManfiestFromRepo(owner, repo, undefined, branch);
}

export const getManifestFromRepo4Impl = toolCache.getManifestFromRepo;
export const findFromManifest3Impl = toolCache.findFromManifest;
export const findFromManifest4Impl = toolCache.findFromManifest;
