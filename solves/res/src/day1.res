open Belt
let input = Node.Fs.readFileAsUtf8Sync("day1.txt")->Js.String2.split("\n")->Belt.Array.map(i => {
  i->Int.fromString->Belt.Option.getWithDefault(0)
})

let part1 = input->Array.map(item => input->Array.partition(x => x + item == 2020))->Array.keep(((
  a,
  _,
)) => {
  a->Array.length > 0
})->Array.map(((a, _)) => {
  switch a->Array.length > 0 {
  | true => a->Array.getExn(0)
  | false => 0
  }
})->Array.reduce(1, (a, b) => a * b)

Js.log(part1)

let part2_arrays = input->Array.map(x => 2020 - x)->Array.map(x => {
  input->Array.map(item => input->Array.partition(y => y + item == x))
})->Array.map(x => x->Array.keep(((a, _)) => {
    a->Array.length > 0
  }))->Array.keep(x => x->Array.length > 0)

part2_arrays->Array.map(x => Js.log(x))
// 802, 956, 262
Js.log(802 * 956 * 262)
