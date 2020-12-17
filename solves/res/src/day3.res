open Belt
let input = Node.Fs.readFileAsUtf8Sync("day3.txt")->Js.String2.split("\n")->Belt.Array.map(i => {
  i
})

type coordinates_t = {
  x: int,
  y: int,
  isForest: bool,
  arrived: bool,
}
type c_t = {
  k: (int, int),
  v: coordinates_t,
}

module CoordinateMap = Belt.Id.MakeComparable({
  type t = (int, int)
  let cmp = compare
})

let l2 =
  input->Array.mapWithIndex((y, line) =>
    line
    ->Js.String2.split("")
    ->Array.mapWithIndex((x, mark) => (
      (x, y),
      {x: x, y: y, isForest: mark == "#", arrived: x == 0 && y == 0},
    ))
  )
let merged = l2->Array.reduce([], (acc, arr) => {
  acc->Array.concat(arr)
})

let mergedMap = Belt.Map.fromArray(merged, ~id=module(CoordinateMap))

let updateCoord = (coord, offset) => {
  let (x, y) = coord
  let (a, b) = offset
  let new_y = y + b
  let new_x = x + a
  (
    switch new_x > 30 {
    | true => new_x - 31
    | false => new_x
    },
    new_y,
  )
}

let updateMap = (map, lastetCoord: (int, int)) => {
  map->Map.update(lastetCoord, v =>
    switch v {
    | Some(v) => Some({x: v.x, y: v.y, isForest: v.isForest, arrived: true})
    | None => None
    }
  )
}
type t = (int, int)
let rec resultMap = (map, coord, ~f: (t, (int, int)) => (int, int), ~offset: (int, int)) => {
  let next: (int, int) = coord->f(offset)
  let v = map->Map.get(coord)
  switch v {
  | None => map
  | Some(_) => map->updateMap(next)->resultMap(next, ~f, ~offset)
  }
}
let get_length = m => {
  m->Map.toArray->Array.keep(coord => {
    let (c, data) = coord
    data.arrived && data.isForest
  })->Array.length
}
let part1_result = mergedMap->resultMap((0, 0), ~f=updateCoord, ~offset=(3, 1))->get_length

let part2_result =
  mergedMap->resultMap((0, 0), ~f=updateCoord, ~offset=(1, 1))->get_length *
  mergedMap->resultMap((0, 0), ~f=updateCoord, ~offset=(3, 1))->get_length *
  mergedMap->resultMap((0, 0), ~f=updateCoord, ~offset=(5, 1))->get_length *
  mergedMap->resultMap((0, 0), ~f=updateCoord, ~offset=(7, 1))->get_length *
  mergedMap->resultMap((0, 0), ~f=updateCoord, ~offset=(1, 2))->get_length

Js.log(part1_result)
Js.log(part2_result)
