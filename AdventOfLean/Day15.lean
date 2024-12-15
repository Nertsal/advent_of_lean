import AdventOfLean.Util

namespace Day15

abbrev Position : Type := Prod Int Int

structure Map where
  size : Position
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

def Map.empty (size : Position) : Map :=
  {
    size := size,
    boxes := [],
    walls := [],
    robot := (-1, -1),
  }

def Map.merge (a : Map) (b : Map) : Map :=
  {
    size := b.size,
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
  | '.' => some <| Map.empty (0, 0)
  | '#' => some <| { Map.empty (0, 0) with walls := [pos] }
  | 'O' => some <| { Map.empty (0, 0) with boxes := [pos] }
  | '@' => some <| { Map.empty (0, 0) with robot := pos }
  | _ => none

def parseMap? (input : List String) : Option Map :=
  let size := ((input.headD "").length, input.length).map Int.ofNat Int.ofNat
  input
  |> List.map String.toList
  |> iterPositions
  |> List.mapM (Util.uncurry parseOne?)
  |> Option.map (fun l => l
      |> List.foldr Map.merge (Map.empty size)
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

def run : IO Unit := Util.run "input/day15.txt" parse? (Util.uncurry solve)

#eval Util.getAnswer "input/day15example.txt" parse? (Util.uncurry solve)
#eval Util.getAnswer "input/day15example2.txt" parse? (Util.uncurry solve)
#eval Util.getAnswer "input/day15example3.txt" parse? (fun (map, moves) =>
  moves
  |> List.foldl step map
  -- |> (fun m => m.boxes
  --     |> List.map (fun p => 100 * p.snd + p.fst)
  --     |> List.foldr Int.add 0
  -- )
)

end Day15
