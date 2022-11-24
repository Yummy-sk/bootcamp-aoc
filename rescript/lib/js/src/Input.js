// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");

function readFile(path) {
  var dirname = ((function () { 
    const path = require('path');
    
    return path.parse(__dirname).dir;
    })());
  var parsedRootPath = dirname.replace(/lib\/js/g, "");
  var restOfPath = path.trim().replace(/^\//, "");
  var fullPath = "" + parsedRootPath + "/" + restOfPath + "";
  return Fs.readFileSync(fullPath, "utf8").trim();
}

function splitLine(str, delim) {
  return str.split(delim);
}

exports.readFile = readFile;
exports.splitLine = splitLine;
/* fs Not a pure module */
