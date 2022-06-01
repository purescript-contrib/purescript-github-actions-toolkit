"use strict";

const glob = require("@actions/glob");

exports.createImpl = glob.create;

exports.getSearchPaths = function (globber) {
  return globber.getSearchPaths();
};

exports.globImpl = function (globber) {
  return globber.glob();
};
