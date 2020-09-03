var cache = require("@actions/cache");

exports.restoreCache2Impl = cache.restoreCache;

exports.restoreCache3Impl = cache.restoreCache;

exports.restoreCache3Impl2 = (paths, primaryKey, options) =>
  cache.restoreCache(paths, primaryKey, undefined, options);

exports.restoreCache4Impl = cache.restoreCache;

exports.saveCache2Impl = cache.saveCache;

exports.saveCache3Impl = cache.saveCache;
