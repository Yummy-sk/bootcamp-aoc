// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Js_array = require("rescript/lib/js/js_array.js");
var Js_string = require("rescript/lib/js/js_string.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_array = require("rescript/lib/js/caml_array.js");
var Caml_int32 = require("rescript/lib/js/caml_int32.js");

function parseInput(param) {
  return Js_string.split("\n", Fs.readFileSync("../../../../input/Week1/Year2020Day3.sample.txt", "utf8").trim());
}

function getFootPrint(_args, _value) {
  while(true) {
    var value = _value;
    var args = _args;
    var row = args.row;
    var input = args.input;
    if (row >= input.length) {
      return value;
    }
    var col = args.col;
    var movement = args.movement;
    var footPrint = Caml_array.get(input, row)[Caml_int32.mod_(col, Caml_array.get(input, row).length)];
    var nextRow = row + movement.down | 0;
    var nextCol = col + movement.right | 0;
    var nextValue = Js_array.concat([footPrint], value);
    _value = nextValue;
    _args = {
      input: args.input,
      movement: args.movement,
      row: nextRow,
      col: nextCol
    };
    continue ;
  };
}

function getTrees(footPrints) {
  return Js_array.filter((function (footPrint) {
                return footPrint === "#";
              }), footPrints);
}

function getNumberOfTrees(trees) {
  return trees.length;
}

function getMultiplyCountOfTrees(trees) {
  return Js_array.reduce((function (acc, tree) {
                return Math.imul(acc, tree);
              }), 1, trees);
}

function solution(input, movements) {
  return getMultiplyCountOfTrees(Belt_Array.map(movements, (function (m) {
                    return getTrees(getFootPrint({
                                    input: input,
                                    movement: m,
                                    row: 0,
                                    col: 0
                                  }, [])).length;
                  })));
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

console.log(solution(parseInput(undefined), movements));

exports.parseInput = parseInput;
exports.getFootPrint = getFootPrint;
exports.getTrees = getTrees;
exports.getNumberOfTrees = getNumberOfTrees;
exports.getMultiplyCountOfTrees = getMultiplyCountOfTrees;
exports.solution = solution;
exports.movements = movements;
/*  Not a pure module */
