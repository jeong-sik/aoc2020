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
Js.log(parsedArray)

let filtered = parsedArray->Array.keep(item => item->Array.length > 6)

let part1 =
  filtered->Array.keep(item =>
    item->Array.reduce("", (a, b) => a->Js.String2.concat(b))->Js.String2.includes("byr") &&
    item->Array.reduce("", (a, b) => a->Js.String2.concat(b))->Js.String2.includes("iyr") &&
    item->Array.reduce("", (a, b) => a->Js.String2.concat(b))->Js.String2.includes("eyr") &&
    item->Array.reduce("", (a, b) => a->Js.String2.concat(b))->Js.String2.includes("hgt") &&
    item->Array.reduce("", (a, b) => a->Js.String2.concat(b))->Js.String2.includes("hcl") &&
    item->Array.reduce("", (a, b) => a->Js.String2.concat(b))->Js.String2.includes("ecl") &&
    item->Array.reduce("", (a, b) => a->Js.String2.concat(b))->Js.String2.includes("pid")
  )

// where is type... gold picker...
Js.log2("Part1 result:", part1->Array.length)
