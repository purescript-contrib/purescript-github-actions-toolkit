const toolCache = require("@actions/tool-cache");

exports.downloadTool1Impl = toolCache.downloadTool;

exports.downloadTool2Impl = toolCache.downloadTool;

exports.downloadTool2Impl2 = (url, auth) => toolCache.downloadTool(url, undefined, auth);

exports.downloadTool3Impl = toolCache.downloadTool;

exports.extract7c1Impl = toolCache.extract7c;

exports.extract7c2Impl = toolCache.extract7c;

exports.extract7c2Impl2 = (file, _7cPath) => toolCache.extract7c(file, undefined, _7cPath);

exports.extract7c3Impl = toolCache.extract7c;

exports.extractTar1Impl = toolCache.extractTar;

exports.extractTar2Impl = toolCache.extractTar;

exports.extractTar2Impl2 = (file, flags) => toolCache.extractTar(file, undefined, flags);

exports.extractTar3Impl = toolCache.extractTar;

exports.extractXar1Impl = toolCache.extractXar;

exports.extractXar2Impl = toolCache.extractXar;

exports.extractXar2Impl2 = (file, flags) => toolCache.extractXar(file, undefined, flags);

exports.extractXar3Impl = toolCache.extractXar;

exports.extractZip1Impl = toolCache.extractZip;

exports.extractZip2Impl = toolCache.extractZip;

exports.cacheDir3Impl = toolCache.cacheDir;

exports.cacheDir4Impl = toolCache.cacheDir;

exports.cacheFile4Impl = toolCache.cacheFile;

exports.cacheFile5Impl = toolCache.cacheFile;

exports.find2Impl = toolCache.findImpl;

exports.find3Impl = toolCache.findImpl;

exports.findAllVersions1Impl = toolCache.findAllVersions;

exports.findAllVersions2Impl = toolCache.findAllVersions;

exports.getManifestFromRepo2Impl = toolCache.getManifestFromRepo;

exports.getManifestFromRepo3Impl = toolCache.getManifestFromRepo;

exports.getManifestFromRepo3Impl2 = (owner, repo, branch) => toolCache.getManfiestFromRepo(owner, repo, undefined, branch);

exports.getManifestFromRepo4Impl = toolCache.getManifestFromRepo;

exports.findFromManifest3Impl = toolCache.findFromManifest;

exports.findFromManifest4Impl = toolCache.findFromManifest;
