import AdventOfLean.Util

namespace Day15

abbrev Position : Type := Prod Int Int

structure Map where
  boxes : List Position
  walls : List Position
  robot : Position
deriving Repr

inductive Direction where
| left : Direction
| right : Direction
| down : Direction
| up : Direction
deriving Repr

def Map.empty : Map :=
  {
    boxes := [],
    walls := [],
    robot := (-1, -1),
  }

def Map.merge (a : Map) (b : Map) : Map :=
  {
    boxes := List.append a.boxes b.boxes,
    walls := List.append a.walls b.walls,
    robot := if b.robot == (-1, -1) then a.robot else b.robot,
  }

def Position.add (a : Position) (b : Position) : Position :=
  (a.fst + b.fst, a.snd + b.snd)

def Direction.toPos : Direction -> Position
  | .left => (-1, 0)
  | .right => (1, 0)
  | .up => (0, -1)
  | .down => (0, 1)

partial def push? (map : Map) (pos : Position) (dir : Direction) : Option Map :=
  if map.walls.contains pos
  then none
  else
    if map.boxes.contains pos
    then
      let next := pos.add dir.toPos
      match push? map next dir with
      | .none => none
      | .some nextMap =>
          let boxes := nextMap.boxes.removeAll [pos]
          some { nextMap with boxes := next :: boxes }
    else
      some map

def step (map : Map) (move : Direction) : Map :=
  let nextPos := map.robot.add move.toPos
  match push? map nextPos move with
  | .none => map
  | .some nextMap => { nextMap with robot := nextPos }

def solve (map : Map) (moves : List Direction) : Int :=
  moves
  |> List.foldl step map
  |> (fun m => m.boxes
      |> List.map (fun p => 100 * p.snd + p.fst)
      |> List.foldr Int.add 0
  )

-- Part 2

def convert (map : Map) : Map :=
  let convertPos := fun pos => (pos.fst * 2, pos.snd)
  {
    walls := map.walls |> List.map convertPos
    boxes := map.boxes |> List.map convertPos
    robot := convertPos (map.robot),
  }

partial def push2? (map : Map) (pos : Position) (dir : Direction) : Option Map :=
  if map.walls.contains pos || map.walls.contains (pos.fst - 1, pos.snd)
  then none
  else
    if map.boxes.contains pos
    then do
      let next1 := pos.add dir.toPos
      let map <- if dir.toPos.fst == 1
        then push2? map (next1.fst + 1, next1.snd) dir
        else push2? map next1 dir
      let map <- if dir.toPos.fst == 0
        then push2? map (next1.fst + 1, next1.snd) dir
        else some map
      let boxes := map.boxes.removeAll [pos]
      some { map with boxes := next1 :: boxes }
    else if map.boxes.contains (pos.fst - 1, pos.snd)
    then do
      let pos : Position := (pos.fst - 1, pos.snd)
      let next1 := pos.add dir.toPos
      let map <- if dir.toPos.fst == 1
        then push2? map (next1.fst + 1, next1.snd) dir
        else push2? map next1 dir
      let map <- if dir.toPos.fst == 0
        then push2? map (next1.fst + 1, next1.snd) dir
        else some map
      let boxes := map.boxes.removeAll [pos]
      some { map with boxes := next1 :: boxes }
    else
      some map

def step2 (map : Map) (move : Direction) : Map :=
  let nextPos := map.robot.add move.toPos
  match push2? map nextPos move with
  | .none => map
  | .some nextMap => { nextMap with robot := nextPos }

def solve2 (map : Map) (moves : List Direction) : Int :=
  let map := convert map
  moves
  |> List.foldl step2 map
  |> (fun m => m.boxes
      |> List.map (fun p => 100 * p.snd + p.fst)
      |> List.foldr Int.add 0
  )

-- Parse

def List.split [BEq a] (sep : a) : List a -> List (List a)
  | [] => []
  | x :: xs => if sep == x
      then [] :: split sep xs
      else match split sep xs with
        | [] => [[x]]
        | ys :: yss => (x :: ys) :: yss

def List.flatten (list : List (List a)) : List a :=
  list
  |> List.foldr List.append []

def enumerate (i : Nat) : List a -> List (Prod Nat a)
  | [] => []
  | x :: xs => (i, x) :: enumerate (i + 1) xs

def iterPositions (input : List (List a)) : List (Prod Position a) :=
  input
  |> enumerate 0
  |> List.map (fun (y, row) =>
    enumerate 0 row
    |> List.map (fun (x, cell) => ((Int.ofNat x, Int.ofNat y), cell))
  )
  |> List.flatten

def parseOne? (pos : Position) : Char -> Option Map
  | '.' => some <| Map.empty
  | '#' => some <| { Map.empty with walls := [pos] }
  | 'O' => some <| { Map.empty with boxes := [pos] }
  | '@' => some <| { Map.empty with robot := pos }
  | _ => none

def parseMap? (input : List String) : Option Map :=
  input
  |> List.map String.toList
  |> iterPositions
  |> List.mapM (Util.uncurry parseOne?)
  |> Option.map (fun l => l
      |> List.foldr Map.merge Map.empty
  )

def parseMoves? (input : List String) : Option (List Direction) :=
  input
  |> List.map (fun s => s.toList |> List.filterMap (fun
      | '<' => some Direction.left
      | '>' => some Direction.right
      | '^' => some Direction.up
      | 'v' => some Direction.down
      | _ => none
  ) )
  |> List.flatten

#eval parseMoves? ["<^^>>>vv<v>>v<<"]

def parse? (input : List String) : Option (Prod Map (List Direction)) :=
  match List.split "" input with
  | [map, moves] => do
      let map <- parseMap? map
      let moves <- parseMoves? moves
      pure (map, moves)
  | _ => none

def run : IO Unit := Util.run "input/day15.txt" parse? (Util.uncurry solve2)

#eval Util.getAnswer "input/day15example.txt" parse? (Util.uncurry solve) -- 2028
#eval Util.getAnswer "input/day15example2.txt" parse? (Util.uncurry solve) -- 10092

#eval Util.getAnswer "input/day15example.txt" parse? (Util.uncurry solve2)
#eval Util.getAnswer "input/day15example2.txt" parse? (Util.uncurry solve2) -- 9021
#eval Util.getAnswer "input/day15example4.txt" parse? (Util.uncurry solve2) -- 618

end Day15
