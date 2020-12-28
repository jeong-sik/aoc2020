open Belt
let input = Node.Fs.readFileAsUtf8Sync("day4.txt")->Js.String2.split("\n")->Belt.Array.map(i => {
  i
})

let parsedList =
  input
  ->Array.map(line => line->String.length == 0 ? "SPLIT" : line)
  ->Array.reduce("", (a, b) => a->Js.String2.concat(" " ++ b))
  ->Js.String2.split("SPLIT")
  ->Array.map(i => i->Js.String2.trim)

let parsedArray = parsedList->Js.Array2.map(passPort => passPort->Js.String2.split(" "))
// Js.log(parsedList->Js.Array2.map(passPort => passPort->Js.Json.parseExn))
// Js.log(parsedArray)

let filtered = parsedArray->Array.keep(item => item->Array.length > 6)

let map = filtered->Array.map(item => {
  item->Array.reduce(Js.Dict.empty(), (a, b) => {
    let keyValue = b->Js.String2.split(":")
    a->Js.Dict.set(keyValue->Js.Array2.unsafe_get(0), keyValue->Js.Array2.unsafe_get(1))
    a
  })
})
// Js.log(map)

@bs.deriving(jsConverter)
type eye_t = [
  | @bs.as("amb")
  #Amb
  | @bs.as("brn")
  #Brn
  | @bs.as("blu")
  #Blu
  | @bs.as("gry")
  #Gry
  | @bs.as("grn")
  #Grn
  | @bs.as("hzl")
  #Hzl
  | @bs.as("oth")
  #Oth
]

let part1 = map->Array.map(item => {
  let byr = item->Js.Dict.get("byr")
  let iyr = item->Js.Dict.get("iyr")
  let eyr = item->Js.Dict.get("eyr")
  let hgt = item->Js.Dict.get("hgt")
  let hcl = item->Js.Dict.get("hcl")
  let ecl = item->Js.Dict.get("ecl")
  let pid = item->Js.Dict.get("pid")

  let dict = Js.Dict.empty()
  dict->Js.Dict.set(
    "byr",
    Js.Json.string(
      switch byr {
      | Some(s) => float_of_string(s) >= 1920.0 && float_of_string(s) <= 2002.0 ? s : ""
      | None => ""
      },
    ),
  )
  dict->Js.Dict.set(
    "iyr",
    Js.Json.string(
      switch iyr {
      | Some(s) => float_of_string(s) >= 2010.0 && float_of_string(s) <= 2020.0 ? s : ""
      | None => ""
      },
    ),
  )
  dict->Js.Dict.set(
    "eyr",
    Js.Json.string(
      switch eyr {
      | Some(s) => float_of_string(s) >= 2020.0 && float_of_string(s) <= 2030.0 ? s : ""
      | None => ""
      },
    ),
  )
  dict->Js.Dict.set(
    "hgt",
    Js.Json.string(
      switch hgt {
      | Some(s) =>
        switch s->Js.String2.includes("in") {
        | true =>
          switch int_of_string(s->Js.String2.replace("in", "")) >= 59 &&
            int_of_string(s->Js.String2.replace("in", "")) <= 76 {
          | true => s
          | false => ""
          }
        | false =>
          switch s->Js.String2.includes("cm") {
          | true =>
            switch int_of_string(s->Js.String2.replace("cm", "")) >= 150 &&
              int_of_string(s->Js.String2.replace("cm", "")) <= 193 {
            | true => s
            | false => ""
            }
          | false => ""
          }
        }
      | None => ""
      },
    ),
  )
  let re = %re("/^#[0-9A-F]{6}$/i")
  dict->Js.Dict.set(
    "hcl",
    Js.Json.string(
      switch hcl {
      | Some(s) =>
        switch Js.Re.exec_(re, s) {
        | Some(r) => s
        | None => ""
        }
      | None => ""
      },
    ),
  )
  dict->Js.Dict.set(
    "ecl",
    Js.Json.string(
      switch ecl {
      | Some(s) =>
        switch s->eye_tFromJs {
        | Some(r) => eye_tToJs(r)
        | None => ""
        }
      | None => ""
      },
    ),
  )
  dict->Js.Dict.set(
    "pid",
    Js.Json.string(
      switch pid {
      | Some(s) => s->String.length == 9 ? s : ""
      | None => ""
      },
    ),
  )
  dict->Js.Json.object_
})
// Js.log(part1)

let p2 = part1->Array.keep(item => {
  let b = item->Js.Json.decodeObject
  let i = item->Js.Json.decodeArray
  // Js.log(b)

  let result = switch b {
  | Some(v) => {
      let byr = v->Js.Dict.get("byr")
      let iyr = v->Js.Dict.get("iyr")
      let eyr = v->Js.Dict.get("eyr")
      let hgt = v->Js.Dict.get("hgt")
      let hcl = v->Js.Dict.get("hcl")
      let ecl = v->Js.Dict.get("ecl")
      let pid = v->Js.Dict.get("pid")

      switch (byr, iyr, eyr, hgt, hcl, ecl, pid) {
      | (Some(a), Some(b), Some(c), Some(d), Some(e), Some(f), Some(g)) =>
        Js.Json.stringify(a)->Js.String2.length == 6 &&
        Js.Json.stringify(b)->Js.String2.length == 6 &&
        Js.Json.stringify(c)->Js.String2.length == 6 &&
        Js.Json.stringify(d)->Js.String2.length > 2 &&
        Js.Json.stringify(e)->Js.String.length == 9 &&
        Js.Json.stringify(f)->Js.String.length == 5 &&
        Js.Json.stringify(g)->Js.String.length == 11
      | _ => false
      }
    }
  | None => true
  }

  result
  // let byr = Js.Json.decodeArray->Js.Dict.get("byr")
  // let iyr = Js.Dict.get("iyr")
  // let eyr = Js.Dict.get("eyr")
  // let hgt = Js.Dict.get("hgt")
  // let hcl = Js.Dict.get("hcl")
  // let ecl = Js.Dict.get("ecl")
  // let pid = Js.Dict.get("pid")
})

// let b = part1->Array.map(i => i->Js.Json.stringify)
// Js.log(b)
// where is type... gold picker...
// Js.log2("Part1 result:", part1->Array.length)

Js.log(p2->Array.length)

// part2 result : 156
