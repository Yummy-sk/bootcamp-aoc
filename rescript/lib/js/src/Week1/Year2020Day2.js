// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");

function getInputData(param) {
  return Fs.readFileSync("../../../../input/Week1/Year2020Day2.sample.txt", "utf8").trim().split("\n");
}

function parseInputData(param) {
  return Belt_Array.map(getInputData(undefined), (function (line) {
                var match = line.split(" ");
                if (match.length !== 3) {
                  throw {
                        RE_EXN_ID: "Match_failure",
                        _1: [
                          "Year2020Day2.res",
                          25,
                          8
                        ],
                        Error: new Error()
                      };
                }
                var range = match[0];
                var letter = match[1];
                var password = match[2];
                var match$1 = range.split("-");
                if (match$1.length !== 2) {
                  throw {
                        RE_EXN_ID: "Match_failure",
                        _1: [
                          "Year2020Day2.res",
                          26,
                          8
                        ],
                        Error: new Error()
                      };
                }
                var lower = match$1[0];
                var upper = match$1[1];
                return {
                        upper: Belt_Option.getExn(Belt_Int.fromString(upper)),
                        lower: Belt_Option.getExn(Belt_Int.fromString(lower)),
                        letter: letter.replace(/:/g, ""),
                        password: password
                      };
              }));
}

function checkLetterCountIsValid(infos) {
  return Belt_Array.map(infos, (function (info) {
                var letterCount = info.password.split(info.letter).length - 1 | 0;
                if (letterCount >= info.lower) {
                  return letterCount <= info.upper;
                } else {
                  return false;
                }
              }));
}

function checkLetterPositionIsValid(infos) {
  return Belt_Array.map(infos, (function (info) {
                var password = info.password;
                var letter = info.letter;
                var letterAtLower = password[info.lower - 1 | 0] === letter;
                var letterAtUpper = password[info.upper - 1 | 0] === letter;
                return letterAtLower !== letterAtUpper;
              }));
}

function countValidPasswords(status) {
  return Belt_Array.keep(status, (function (x) {
                return x;
              })).length;
}

function solution(partType) {
  if (partType) {
    var status = checkLetterPositionIsValid(parseInputData(undefined));
    console.log(Belt_Array.keep(status, (function (x) {
                return x;
              })).length);
    return ;
  }
  var status$1 = checkLetterCountIsValid(parseInputData(undefined));
  console.log(Belt_Array.keep(status$1, (function (x) {
              return x;
            })).length);
  
}

solution(/* Part1 */0);

solution(/* Part2 */1);

exports.getInputData = getInputData;
exports.parseInputData = parseInputData;
exports.checkLetterCountIsValid = checkLetterCountIsValid;
exports.checkLetterPositionIsValid = checkLetterPositionIsValid;
exports.countValidPasswords = countValidPasswords;
exports.solution = solution;
/*  Not a pure module */
