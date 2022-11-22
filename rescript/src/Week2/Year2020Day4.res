type part = Part1 | Part2

type ecl = 
  Amb | 
  Blu | 
  Grn | 
  Gry | 
  Hzl | 
  Oth | 
  Brn | 
  Unknown | 
  None

type hcl = Hcl(string) | None

type pid = Pid(string) | None

type hgt = Cm(int) | In(int) | Unknown | None

type passport = {
  byr: int,
  iyr: int,
  eyr: int,
  hcl: hcl,
  hgt: hgt,
  ecl: ecl,
  pid: pid,
  cid?: int, 
}


let covertTypeToInt = (value) =>
  value->Belt.Int.fromString->Belt.Option.getExn
 

let formatPassportInfo = (line) => {
  let initialPassport = {
    byr: 0,
    iyr: 0,
    eyr: 0,
    hcl: None,
    hgt: None,
    ecl: None,
    pid: None,
  }

  line
  ->Js.String2.split(" ")
  ->Belt.Array.reduce(initialPassport, (acc, info) => {

    switch (info->Js.String2.split(":")) {
    | [key, value] => {
      switch (key) {
      | "byr" => {...acc, byr: value->covertTypeToInt}
      | "iyr" => {...acc, iyr: value->covertTypeToInt}
      | "eyr" => {...acc, eyr: value->covertTypeToInt}
      | "hcl" => {...acc, hcl: Hcl(value)}
      | "hgt" => {
          let unit = value->Js.String2.sliceToEnd(~from=-2)
          let height = value->Js.String2.slice(~from=0, ~to_=-2)

          switch (unit) {
          | "cm" => {...acc, hgt: Cm(height->covertTypeToInt)}
          | "in" => {...acc, hgt: In(height->covertTypeToInt)}
          | _ => {...acc, hgt: Unknown}
          }
      }
      | "ecl" => {...acc, ecl: switch (value) {
        | "amb" => Amb
        | "blu" => Blu
        | "brn" => Brn
        | "gry" => Gry
        | "grn" => Grn
        | "hzl" => Hzl
        | "oth" => Oth
        | _ => Unknown
        }}
      | "pid" => {...acc, pid: Pid(value)}
      | "cid" => {...acc, cid: value->covertTypeToInt}
      | _ => acc
      }
    }
    | _ => acc}
  })
}


let parsePassport = () => {
  Input.readFile("input/Week2/Year2020Day4.sample.txt")
  ->Js.String2.split("\n\n")
  ->Belt.Array.map(line=>
    line
    ->Js.String2.replaceByRe(%re("/\n/g"), " ")
    ->Js.String2.trim
  )
  ->Belt.Array.map(formatPassportInfo)
}


let getPassportWhichFieldAreAllExist = (passports) => {

  passports
  ->Belt.Array.keep(passport => {
    let {byr, iyr, eyr, hgt, hcl, ecl, pid} = passport

    byr > 0 && 
    iyr > 0 && 
    eyr > 0 && 
    hgt != None &&
    hcl != None && 
    ecl != None && 
    pid != None
  })
}


let rangeValidator = (min, max, value) =>
  value >= min && value <= max


let checkPassportFieldsAreValid = (passports) => {
  passports
  ->Belt.Array.keep(passport => {
    let {byr, iyr, eyr, hgt, hcl, ecl, pid} = passport

    rangeValidator(1920, 2002, byr) &&
    rangeValidator(2010, 2020, iyr) &&
    rangeValidator(2020, 2030, eyr) &&
    switch (hgt) {
    | Cm(height) => height >= 150 && height <= 193
    | In(height) => height >= 59 && height <= 76
    | _ => false
    } &&
    switch (hcl) {
    | Hcl(value) => Js.Re.test_(%re("/^#[0-9a-f]{6}$/"), value)
    | _ => false
    } &&
    switch (ecl) {
    | Amb => true
    | Blu => true
    | Brn => true
    | Gry => true
    | Grn => true
    | Hzl => true
    | Oth => true
    | _ => false
    } &&
    switch (pid) {
    | Pid(value) => Js.Re.test_(%re("/^[0-9]{9}$/"), value)
    | _ => false
    }
  })
}


let countPassport = (passports, part) => {
  switch (part) {
    | Part1 => passports->getPassportWhichFieldAreAllExist->Belt.Array.length
    | Part2 => passports->checkPassportFieldsAreValid->Belt.Array.length
  }
}


let solution = (part) => 
  switch(part) {
    | Part1 => parsePassport()->countPassport(Part1)->Js.log
    | Part2 => parsePassport()->countPassport(Part2)->Js.log
  }


Part1->solution
Part2->solution
