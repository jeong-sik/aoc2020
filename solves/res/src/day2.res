open Belt
let input = Node.Fs.readFileAsUtf8Sync("day2.txt")->Js.String2.split("\n")->Belt.Array.map(i => {
  i->Js.String2.split(" ")
})

type password_t = {
  min: int,
  max: int,
  char: string,
  password: string,
}

let passwords: array<password_t> = input->Array.map(item => {
  let range = item->Array.getExn(0)->Js.String2.split("-")
  let char = item->Array.getExn(1)->Js.String.get(0)
  let password = item->Array.getExn(2)

  {
    min: range->Array.getExn(0)->int_of_string,
    max: range->Array.getExn(1)->int_of_string,
    char: char,
    password: password,
  }
})

let part1 = passwords->Array.keep(pass => {
  let passLength =
    pass.password->Js.String2.split("")->Belt.Array.keep(c => c == pass.char)->Belt.Array.length

  passLength >= pass.min && passLength <= pass.max
})

Js.log(part1->Array.length)
//part1

let part2 = passwords->Array.keep(pass => {
  let first = pass.password->String.get(pass.min - 1)
  let second = pass.password->String.get(pass.max - 1)

  let c = pass.char->String.get(0)
  (first == c && second != c) || (first != c && second == c)
})
Js.log(part2->Array.length)
//part2
