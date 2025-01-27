// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Js_string = require("rescript/lib/js/js_string.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Caml_splice_call = require("rescript/lib/js/caml_splice_call.js");

function getInputData(param) {
  return Js_string.split("\n", Fs.readFileSync("../../../../input/Week1/Year2020Day5.sample.txt", "utf8").trim());
}

function getBinaryNumber(input) {
  return input.split("").map(function (x) {
                switch (x) {
                  case "F" :
                  case "L" :
                      return "0";
                  case "B" :
                  case "R" :
                      return "1";
                  default:
                    throw {
                          RE_EXN_ID: "Invalid_argument",
                          _1: "Invalid input",
                          Error: new Error()
                        };
                }
              }).join("");
}

function getDecimalNumber(binary) {
  return Number("0b" + binary + "") | 0;
}

function calculateSeatId(inputs) {
  return inputs.map(function (input) {
              return getDecimalNumber(getBinaryNumber(input));
            });
}

function getMaxValue(inputs) {
  return Caml_splice_call.spliceApply(Math.max, [inputs]);
}

function checkSeatIsExist(inputs, seatId) {
  return inputs.includes(seatId);
}

function getMissingSeatId(inputs) {
  return Belt_Option.getExn(Belt_Option.map(Caml_option.undefined_to_opt(inputs.find(function (input) {
                          if (inputs.includes(input + 1 | 0)) {
                            return false;
                          } else {
                            return inputs.includes(input + 2 | 0);
                          }
                        })), (function (x) {
                    return x + 1 | 0;
                  })));
}

function solutionPart1(param) {
  console.log(Caml_splice_call.spliceApply(Math.max, [calculateSeatId(getInputData(undefined))]));
}

function solutionPart2(param) {
  console.log(getMissingSeatId(calculateSeatId(getInputData(undefined))));
}

solutionPart1(undefined);

solutionPart2(undefined);

exports.getInputData = getInputData;
exports.getBinaryNumber = getBinaryNumber;
exports.getDecimalNumber = getDecimalNumber;
exports.calculateSeatId = calculateSeatId;
exports.getMaxValue = getMaxValue;
exports.checkSeatIsExist = checkSeatIsExist;
exports.getMissingSeatId = getMissingSeatId;
exports.solutionPart1 = solutionPart1;
exports.solutionPart2 = solutionPart2;
/*  Not a pure module */
