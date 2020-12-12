open Belt
let input = Node.Fs.readFileAsUtf8Sync("day1.txt")->Js.String2.split("\n")->Belt.Array.map(i => {
  i->Int.fromString->Belt.Option.getWithDefault(0)
})
let sortedInput = input->Belt.SortArray.stableSortBy((a, b) => a - b)
let arrays = input->Array.map(item => input->Array.partition(x => x + item == 2020))

let sums = arrays->Array.keep(x => {
  let (a, _) = x
  a->Array.length > 0
})

let r = sums->Array.map(x => {
  let (a, _) = x
  switch a->Array.length > 0 {
  | true => a->Array.getExn(0)
  | false => 0
  }
})
let part1 = r->Array.reduce(1, (a, b) => a * b)
Js.log(part1)

let offsets = sortedInput->Array.map(x => 2020 - x)

let part2_arrays = offsets->Array.map(x => {
  input->Array.map(item => input->Array.partition(y => y + item == x))
})

let filtered = part2_arrays->Array.map(x => x->Array.keep(y => {
    let (a, _) = y
    a->Array.length > 0
  }))

let r = filtered->Array.keep(x => x->Array.length > 0)

Js.log(r)
r->Array.map(x => Js.log(x))
// 802, 956, 262
Js.log(802 * 956 * 262)
