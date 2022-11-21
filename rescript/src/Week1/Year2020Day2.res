
type passwordInfo = {
  upper: int,
  lower: int,
  letter: string,
  password: string
}


type partType = Part1 | Part2


let getInputData = () => 
  Node.Fs.readFileAsUtf8Sync("../../../../input/Week1/Year2020Day2.sample.txt") 
  ->Js.String2.trim
  ->Js.String2.split("\n")

// 원하는 입력값을 위해 파싱하는 함수
// 1-3 a: abcde -> {upper: 1, lower: 3, letter: "a", password: "abcde"}
let parseInputData = () => {
  open Belt

  getInputData()
  ->Array.map(line => {
    let [range, letter, password] = Js.String2.split(line, " ")
    let [lower, upper] = Js.String2.split(range, "-")
    {
      upper: Int.fromString(upper)->Option.getExn,
      lower: Int.fromString(lower)->Option.getExn,
      letter: letter->Js.String2.replaceByRe(%re("/:/g"), ""),
      password: password
    }
  })
}

// part1을 위해 해당 letter가 최소 및 최대 범위에 있는지 확인하는 함수
let checkLetterCountIsValid = (infos) => {
  open Belt

  infos
  ->Array.map(info => {
    let {upper, lower, letter, password} = info
    let letterCount = password->Js.String2.split(letter)->Array.length - 1

    letterCount >= lower && letterCount <= upper
  })
}

// part2를 위해 각 포지션에 유효한 letter가 있는지 확인하는 함수
let checkLetterPositionIsValid = (infos) => {
    open Belt

    infos
    ->Array.map(info => {
      let {upper, lower, letter, password} = info
      let letterAtLower = password->Js.String2.get(lower - 1) == letter
      let letterAtUpper = password->Js.String2.get(upper - 1) == letter 

      letterAtLower != letterAtUpper
    })
}

// 유요한 비밀번호 갯수를 카운트 합니다.
let countValidPasswords = (status) => 
  status
  ->Belt.Array.keep(x => x)
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
