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
  return Fs.readFileSync(parsedRootPath + "/" + restOfPath, "utf8");
}

exports.readFile = readFile;
/* fs Not a pure module */
