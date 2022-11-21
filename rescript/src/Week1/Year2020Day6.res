module StrCmp =
  Belt.Id.MakeComparable({
    type t = string;
    let cmp = Pervasives.compare
  })


type solType = 
| Solution1
| Solution2


type setType = 
| Union
| Intersection



let parseInput = () => {
  Input.readFile("input/Week1/Year2020Day6.sample.txt")
    ->Js.String2.trim
    ->Js.String2.split("\n\n")
    ->Js.Array2.map((str) => str->Js.String2.split("\n"))
    ->Belt.Array.map((arr) => arr->Belt.Array.map((str)=>str->Js.String2.split("")))
}

// 각 파트에 맞는 값을 도출하는 코드입니다.
let handleSet = (groups, setType) => {
  open Belt

  groups
  ->Array.map((group) => {
    group
    ->Array.reduce(group->Array.get(0), (acc, person) => {
      switch acc {
        | Some(acc) => {
          let prev = Set.fromArray(acc, ~id=module(StrCmp))
          let curr = Set.fromArray(person, ~id=module(StrCmp))

          switch setType {
            | Union => Set.union(prev, curr)->Set.toArray->Some // 합집합 for part1
            | Intersection => Set.intersect(prev, curr)->Set.toArray->Some // 교집합 for part2
          }
        }
        | None => None
      }
    })
  }->Option.getExn)
}

// 각 파트에 맞는 값을 파싱하는 코드입니다. (parse)
let parseData = (solType) => {
  let groups = parseInput()
  switch solType {
    | Solution1 => groups->handleSet(Union)
    | Solution2 => groups->handleSet(Intersection)
  }
}

// 파싱된 데이터에 문제를 위한 값을 반환하는 함수입니다. (process)
let countAnswers = (groups) => {
  open Belt
  groups
    ->Array.map((group) => group->Array.length)
}

// 전달 받은 값의 합계를 집계하는 함수입니다. (aggregate)
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
