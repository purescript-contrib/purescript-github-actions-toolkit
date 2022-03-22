import cache from "@actions/cache";
export const restoreCache2Impl = cache.restoreCache;
export const restoreCache3Impl = cache.restoreCache;

export function restoreCache3Impl2(paths, primaryKey, options) {
  return cache.restoreCache(paths, primaryKey, undefined, options);
}

export const restoreCache4Impl = cache.restoreCache;
export const saveCache2Impl = cache.saveCache;
export const saveCache3Impl = cache.saveCache;
