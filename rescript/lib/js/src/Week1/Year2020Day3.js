// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Caml_array = require("rescript/lib/js/caml_array.js");
var Caml_int32 = require("rescript/lib/js/caml_int32.js");

var input = Fs.readFileSync("../../../../input/Week1/Year2020Day3.sample.txt", "utf8").trim().split("\n");

function getFootPrint(input, movement, row, col) {
  if (row >= input.length) {
    return [];
  }
  var footPrint = Caml_array.get(input, row)[Caml_int32.mod_(col, Caml_array.get(input, row).length)];
  var nextRow = row + movement.down | 0;
  var nextCol = col + movement.right | 0;
  return getFootPrint(input, movement, nextRow, nextCol).concat([footPrint]);
}

function getTrees(footPrints) {
  return footPrints.filter(function (footPrint) {
              return footPrint === "#";
            });
}

function getNumberOfTrees(trees) {
  return trees.length;
}

function getSumOfTrees(trees) {
  return trees.reduce((function (acc, tree) {
                return Math.imul(acc, tree);
              }), 1);
}

function solution(input, movements) {
  return getSumOfTrees(movements.map(function (movement) {
                  return getTrees(getFootPrint(input, movement, 0, 0)).length;
                }));
}

var movements = [
  {
    right: 1,
    down: 1
  },
  {
    right: 3,
    down: 1
  },
  {
    right: 5,
    down: 1
  },
  {
    right: 7,
    down: 1
  },
  {
    right: 1,
    down: 2
  }
];

var result = solution(input, movements);

console.log(result);

exports.input = input;
exports.getFootPrint = getFootPrint;
exports.getTrees = getTrees;
exports.getNumberOfTrees = getNumberOfTrees;
exports.getSumOfTrees = getSumOfTrees;
exports.solution = solution;
exports.movements = movements;
exports.result = result;
/* input Not a pure module */
