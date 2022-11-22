
type passwordInfo = {
  upper: int,
  lower: int,
  letter: string,
  password: string
}

type partType = Part1 | Part2


let getInputData = () => 
  Input.readFile("input/Week1/Year2020Day2.sample.txt") 
  ->Js.String2.split("\n")


let formatData = (line) => {
  open Belt
  switch (line->Js.String2.split(" ")) {
    | [range, letter, password] => {

      switch (range->Js.String2.split("-")) {
        | [lower, upper] => {
          {
            upper: upper->Int.fromString->Option.getExn,
            lower: lower->Int.fromString->Option.getExn,
            letter: letter->Js.String2.replaceByRe(%re("/:/g"), ""),
            password
          }
        }
        | _ => failwith("Invalid range")
      }
    }
    | _ => failwith("Invalid line")
  }
}

// 원하는 입력값을 위해 파싱하는 함수
// 1-3 a: abcde -> {lower: 1, upper: 3, letter: "a", password: "abcde"}
let parseInputData = () => {
  open Belt

  getInputData()
  ->Array.map(line => line->formatData)
}


// part1을 위해 해당 letter가 최소 및 최대 범위에 있는지 확인하는 함수
let checkLetterCountIsValid = (infos) => {
  open Belt

  infos
  ->Array.keep(info => {
    let {upper, lower, letter, password} = info
    let letterCount = password->Js.String2.split("")->Array.keep(x => x === letter)->Array.length

    letterCount >= lower && letterCount <= upper
  })
}


// part2를 위해 각 포지션에 유효한 letter가 있는지 확인하는 함수
let checkLetterPositionIsValid = (infos) => {
    open Belt

    infos
    ->Array.keep(info => {
      let {upper, lower, letter, password} = info
      let letterAtLower = password->Js.String2.get(lower - 1) == letter
      let letterAtUpper = password->Js.String2.get(upper - 1) == letter 

      letterAtLower != letterAtUpper
    })
}

// 유요한 비밀번호 갯수를 카운트 합니다.
let countValidPasswords = (status) => 
  status
  ->Belt.Array.length


let solution = (partType) =>
  switch partType {
    | Part1 => {
        parseInputData() // parse input data
        ->checkLetterCountIsValid // check if letter count is valid
        ->countValidPasswords // count valid passwords
        ->Js.log // log result
    }
    | Part2 => {
        parseInputData()  // parse input data
        ->checkLetterPositionIsValid // check if letter count is valid
        ->countValidPasswords // count valid passwords
        ->Js.log // log result
    }
  }


Part1->solution
Part2->solution
