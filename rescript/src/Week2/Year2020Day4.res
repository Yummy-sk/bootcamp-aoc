/**  Part 1  */


// 필드의 유무만 판단하면 되기 때문에 애초에 값을 result 타입으로 string 또는 int로 구성되도록 하였습니다.
type passport1 = {
  byr: option<int>,
  iyr: option<int>,
  eyr: option<int>,
  hcl: option<string>,
  hgt: option<string>,
  ecl: option<string>,
  pid: option<string>,
  cid: option<int>
}


let covertTypeToInt = (value) =>
  value->Belt.Int.fromString->Belt.Option.getExn


// Passport에 대해 레코드로 변환하여 반환하는 코드입니다. for Part1
let getParsedPassportForPart1 = (passport) => {
  let initialRecord = {
    byr: None,
    iyr: None,
    eyr: None,
    hcl: None,
    hgt: None,
    ecl: None,
    pid: None,
    cid: None
  }

  passport
  ->Belt.Array.reduce(initialRecord, (acc, (key, value)) => {
    switch key {
    | "byr" => { ...acc, byr: Some(value->covertTypeToInt) }
    | "iyr" => { ...acc, iyr: Some(value->covertTypeToInt) }
    | "eyr" => { ...acc, eyr: Some(value->covertTypeToInt) }
    | "hcl" => { ...acc, hcl: Some(value) }
    | "hgt" => { ...acc, hgt: Some(value) }
    | "ecl" => { ...acc, ecl: Some(value) }
    | "pid" => { ...acc, pid: Some(value) }
    | "cid" => { ...acc, cid: Some(value->covertTypeToInt) }
    | _ => acc
    }
  })
}


// Part 1을 위한 파서 입니다. 
let parseRecordForPart1 = (passports) => 
  passports
  ->Belt.Array.map((passport) => {
    passport
    ->getParsedPassportForPart1
  })



/**  Part 2  */

type ecl = 
  Amb | 
  Blu | 
  Grn | 
  Gry | 
  Hzl | 
  Oth | 
  Brn

type hcl = Hcl(string)

type pid = Pid(string)

type hgt = Cm(int) | In(int)

// Part2의 경우 값의 타입을 체크해야 하기 때문에 result 타입으로 string이 아닌 각각의 타입으로 구성되도록 하였습니다.
type passport2 = {
  byr: result<int, string>,
  iyr: result<int, string>,
  eyr: result<int, string>,
  hcl: result<hcl, string>,
  hgt: result<hgt, string>,
  ecl: result<ecl, string>,
  pid: result<pid, string>,
  cid: result<int, string>
}


let rangeChecker = (~min, ~max, ~value) => {
  switch (value >= min && value <= max) {
  | true => Ok(value)
  | false => Error("Invalid range")
  }
}


// Passport에 대해 레코드로 변환하여 반환하는 코드입니다. for Part2
let getParsedPassportForPart2 = (passport) => {
  let initialRecord = {
    byr: Error(""),
    iyr: Error(""),
    eyr: Error(""),
    hcl: Error(""),
    hgt: Error(""),
    ecl: Error(""),
    pid: Error(""),
    cid: Error("")
  }

  passport
  ->Belt.Array.reduce(initialRecord, (acc, (key, value)) => {
      switch key {
      | "byr" => { ...acc, byr: rangeChecker(~min=1920, ~max=2002, ~value=value->covertTypeToInt) }
      | "iyr" => { ...acc, iyr: rangeChecker(~min=2010, ~max=2020, ~value=value->covertTypeToInt) }
      | "eyr" => { ...acc, eyr: rangeChecker(~min=2020, ~max=2030, ~value=value->covertTypeToInt) }
      | "hcl" => {
        switch (Js.Re.test_(%re("/^#[0-9a-f]{6}$/"), value)) {
        | true => { ...acc, hcl: Ok(Hcl(value)) }
        | false => { ...acc, hcl: Error("Invalid hcl") }
        }
      }
      | "hgt" => {
          let unit = value->Js.String2.sliceToEnd(~from=-2)
          let height = value->Js.String2.slice(~from=0, ~to_=-2)
        switch unit {
        | "cm" => { 
          ...acc, 
          hgt: rangeChecker(~min=150, ~max=193, ~value=height->covertTypeToInt)
               ->Belt.Result.map((height) => In(height))
          }
        | "in" => { 
          ...acc, 
          hgt: rangeChecker(~min=59, ~max=76, ~value=height->covertTypeToInt)
                ->Belt.Result.map((height) => In(height))
        }
        | _ => { ...acc, hgt: Error("Invalid unit") }
        }
      }
      | "ecl" => {
        switch value {
        | "amb" => { ...acc, ecl: Ok(Amb) }
        | "blu" => { ...acc, ecl: Ok(Blu) }
        | "brn" => { ...acc, ecl: Ok(Brn) }
        | "gry" => { ...acc, ecl: Ok(Gry) }
        | "grn" => { ...acc, ecl: Ok(Grn) }
        | "hzl" => { ...acc, ecl: Ok(Hzl) }
        | "oth" => { ...acc, ecl: Ok(Oth) }
        | _ => { ...acc, ecl: Error("Invalid ecl") }
        }
      }
      | "pid" => {
        switch (Js.Re.test_(%re("/^[0-9]{9}$/"), value)) {
        | true => { ...acc, pid: Ok(Pid(value)) }
        | false => { ...acc, pid: Error("Invalid pid") }
        }
      }
      | "cid" => { ...acc, cid: Ok(value->covertTypeToInt) }
      | _ => acc
    }
  })
}


// Part 2를 위한 파서 입니다.
let parseRecordForPart2 = (passports) => 
  passports
  ->Belt.Array.map((passport) => {
    passport
    ->getParsedPassportForPart2
  })


let splitToKeyAndValue = (fields) => 
  fields
  ->Js.String2.split(" ")
  ->Belt.Array.map((field) => {
    let pair = field->Js.String2.split(":");

    switch pair {
    | [key, value] => (key, value)
    | _ => raise (Invalid_argument("Invalid field"))
    }
  })


let getInput = () => {
  Input.readFile("input/Week2/Year2020Day4.sample.txt")
  ->Input.splitLine(~delim="\n\n")
  ->Belt.Array.map(line=>
    line
    ->Js.String2.replaceByRe(%re("/\n/g"), " ")
    ->splitToKeyAndValue
  )
}


let parsePassport1 = () =>  {
  getInput()
  ->parseRecordForPart1
}


let parsePassport2 = () =>  {
  getInput()
  ->parseRecordForPart2
}


type validatePassport1 = (array<passport1>) => array<passport1> 
let validatePassport1: validatePassport1 = (passports) => {
  passports
  ->Belt.Array.keep(passport => {
    let {byr, iyr, eyr, hcl, hgt, ecl, pid} = passport;

    switch (byr, iyr, eyr, hcl, hgt, ecl, pid) {
      | (Some(_), Some(_), Some(_), Some(_), Some(_), Some(_), Some(_)) => true
      | _ => false
    }
  })
}


type validatePassport2 = (array<passport2>) => array<passport2> 
let validatePassport2: validatePassport2 = (passports) => {
  passports
  ->Belt.Array.keep(passport => {
    switch passport {
    | {byr: Ok(_), iyr: Ok(_), eyr: Ok(_), hcl: Ok(_), hgt: Ok(_), ecl: Ok(_), pid: Ok(_), _} => true
    | _ => false
    }
  })
}

/**
* 이 부분의 중복은 어떻게 못할까요..? ㅠ
*/


parsePassport1()
->validatePassport1
->Belt.Array.length
->Js.log


parsePassport2()  
->validatePassport2
->Belt.Array.length
->Js.log
