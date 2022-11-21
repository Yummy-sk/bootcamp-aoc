
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

let checkLetterCountIsValid = (infos) => {
  open Belt

  infos
  ->Array.map(info => {
    let {upper, lower, letter, password} = info
    let letterCount = password->Js.String2.split(letter)->Array.length - 1

    letterCount >= lower && letterCount <= upper
  })
}

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


let countValidPasswords = (status) => 
  status
  ->Belt.Array.keep(x => x)
  ->Belt.Array.length


let solution = (partType) =>
  switch partType {
    | Part1 => {
        parseInputData()
        ->checkLetterCountIsValid
        ->countValidPasswords
        ->Js.log      
    }
    | Part2 => {
        parseInputData()
        ->checkLetterPositionIsValid
        ->countValidPasswords
        ->Js.log
    }
  }


Part1->solution
Part2->solution
