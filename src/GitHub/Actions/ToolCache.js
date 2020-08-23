const toolCache = require("@actions/tool-cache");

exports.downloadToolImpl = ({ url, dest, auth }) => () =>
  toolCache.downloadTool(url, dest, auth);

exports.extract7cImpl = ({ file, dest, _7zPath }) => () =>
  toolCache.extract7c(file, dest, _7zPath);

exports.extractTar1Impl = (file) =>
  toolCache.extractTar(file);

exports.extractXarImpl = ({ file, dest, flags }) => () =>
  toolCache.extractXar(file, dest, flags);

exports.extractZipImpl = ({ file, dest }) => () =>
  toolCache.extractZip(file, dest);

exports.cacheDirImpl = ({ sourceDir, tool, version, arch }) => () =>
  toolCache.cacheDir(sourceDir, tool, version, arch);

exports.cacheFileImpl = ({ sourceFile, targetFile, tool, version }) => () =>
  toolCache.cacheFile(sourceFile, targetFile, tool, version);

exports.findImpl = ({ toolName, versionSpec, arch }) => () =>
  toolCache.find(toolName, versionSpec, arch);

exports.findAllVersionsImpl = ({ toolName, arch }) => () =>
  toolCache.findAllVersions(toolName, arch);

exports.getManifestFromRepoImpl = ({ owner, repo, auth, branch }) => () =>
  toolCache.getManifestFromRepo(owner, repo, auth, branch);

exports.findFromManifestImpl = ({ versionSpec, stable, manifest, archFilter }) => () =>
  toolCache.findFromManifest(versionSpec, stable, manifest, archFilter);
