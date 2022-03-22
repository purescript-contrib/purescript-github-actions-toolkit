"use strict";
import cache from "@actions/cache";
export var restoreCache2Impl = cache.restoreCache;
export var restoreCache3Impl = cache.restoreCache;

export function restoreCache3Impl2(paths, primaryKey, options) {
  return cache.restoreCache(paths, primaryKey, undefined, options);
}

export var restoreCache4Impl = cache.restoreCache;
export var saveCache2Impl = cache.saveCache;
export var saveCache3Impl = cache.saveCache;
