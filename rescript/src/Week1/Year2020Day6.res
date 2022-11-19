module StrCmp =
  Belt.Id.MakeComparable({
    type t = string;
    let cmp = Pervasives.compare
  })

type solType = 
| Solution1
| Solution2

let parseInput = () => {
  Node.Fs.readFileAsUtf8Sync("../../../../input/Week1/Year2020Day6.sample.txt") 
    ->Js.String2.trim
    ->Js.String2.split("\n\n")
    ->Js.Array2.map((str) => str->Js.String2.split("\n"))
    ->Belt.Array.map((arr) => arr->Belt.Array.map((str)=>str->Js.String2.split("")))
}


let makeUnique = (groups) => {
  open Belt
  groups
    ->Array.map((group) => 
      group
        ->Set.String.fromArray
        ->Set.String.toArray
    )
}


let getAllAnsweredQuestions = (groups) => {
  open Belt
  groups
  ->Array.map((group) => {
      group
      ->Array.reduce(group->Array.get(0), (acc, person) => {
        switch(acc) {
          | Some(acc) => {
            let prev = Set.fromArray(acc, ~id=module(StrCmp))
            let curr = Set.fromArray(person, ~id=module(StrCmp))

            Set.intersect(prev, curr)
            ->Set.toArray
            ->Some
          }
          | None => None
        }
      })
  }
  ->Option.getExn)
}


let parseData = (solType) => {
  open Belt
  switch (solType) {
    | Solution1 => 
      parseInput()
        ->Array.map(Array.concatMany)
        ->makeUnique
    | Solution2 => {
      parseInput()
        ->getAllAnsweredQuestions
    }
  }
}


let countAnswers = (groups) => {
  groups
    ->Belt.Array.map((group) => group->Belt.Array.length)
}


let getSumOfCounts = (counts) => 
  counts
    ->Belt.Array.reduce(0, (acc, count) => acc + count)


let solve = (solType) => {
  parseData(solType) // parse
    ->countAnswers // process
    ->getSumOfCounts // aggregate
    ->Js.log // print
}


let solution = (solType) => {
  switch(solType) {
    | Solution1 => solve(Solution1)
    | Solution2 => solve(Solution2)
  }
}


solution(Solution1)
solution(Solution2)
