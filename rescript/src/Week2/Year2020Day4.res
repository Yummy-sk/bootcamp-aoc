
type ecl = 
  Amb(string) | 
  Blu(string) | 
  Grn(string) | 
  Gry(string) | 
  Hzl(string) | 
  Oth(string) | 
  Brn(string) | 
  Unknown(string) | 
  None
type hcl = Hcl(string) | None
type pid = Pid(string) | None


type passport = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: int,
  hcl: hcl,
  ecl: ecl,
  pid: pid,
  cid?: int, 
}

let formatPassportInfo = (line) => {
  let initialPassport = {
    byr: 0,
    iyr: 0,
    eyr: 0,
    hgt: 0,
    hcl: None,
    ecl: None,
    pid: None,
  }

  line
  ->Js.String2.split(" ")
  ->Belt.Array.reduce(initialPassport, (acc, info) => {

    switch (info->Js.String2.split(":")) {
    | [key, value] => {
      switch (key) {
      | "byr" => {...acc, byr: value->Belt.Int.fromString->Belt.Option.getExn}
      | "iyr" => {...acc, iyr: value->Belt.Int.fromString->Belt.Option.getExn}
      | "eyr" => {...acc, eyr: value->Belt.Int.fromString->Belt.Option.getExn}
      | "hgt" => {...acc, hgt: value->Belt.Int.fromString->Belt.Option.getExn}
      | "hcl" => {...acc, hcl: Hcl(value)}
      | "ecl" => {...acc, ecl: switch (value) {
        | "amb" => Amb(value)
        | "blu" => Blu(value)
        | "brn" => Brn(value)
        | "gry" => Gry(value)
        | "grn" => Grn(value)
        | "hzl" => Hzl(value)
        | "oth" => Oth(value)
        | _ => Unknown(value)
        }}
      | "pid" => {...acc, pid: Pid(value)}
      | "cid" => {...acc, cid: value->Belt.Int.fromString->Belt.Option.getExn}
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

let checkAllFieldsAreExist = (passport) => {
  let {byr, iyr, eyr, hgt, hcl, ecl, pid} = passport

  byr > 0 && 
  iyr > 0 && 
  eyr > 0 && 
  hgt > 0 && 
  hcl != None && 
  ecl != None && 
  pid != None
}



let countPassport = (passports) => {
  passports
  ->Belt.Array.keep(passport => passport->checkAllFieldsAreExist)
  ->Belt.Array.length
}



parsePassport()
->countPassport
->Js.log
