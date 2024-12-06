import AdventOfLean.Util

namespace Day06

inductive Direction where
  | left : Direction
  | right : Direction
  | down : Direction
  | up : Direction
deriving Repr, BEq

def Direction.rotate : Direction -> Direction
  | .left => up
  | .up => right
  | .right => down
  | .down => left

structure Guard where
  x : Nat
  y : Nat
  dir : Direction
deriving Repr, BEq

def Guard.nextPos (guard : Guard) : Prod Int Int :=
  match guard.dir with
  | Direction.left => (guard.x - 1, guard.y)
  | Direction.right => (guard.x + 1, guard.y)
  | Direction.down => (guard.x, guard.y - 1)
  | Direction.up => (guard.x, guard.y + 1)

def step (map : Array (Array Bool)) (guard : Guard) : Option Guard := do
  let target := guard.nextPos
  let targetX <- if target.fst < 0 || target.fst >= (map.get! 0).size
    then none
    else some target.fst.toNat
  let targetY <- if target.snd < 0 || target.snd >= map.size
    then none
    else some target.snd.toNat
  let cell := (map.get! targetY).get! targetX
  match cell with
  | false => some { guard with x := targetX, y := targetY }
  | true => some { guard with dir := guard.dir.rotate }

inductive Cell where
  | empty : Cell
  | full : Cell
  | guard (dir : Direction) : Cell
deriving Repr

def iterPositions (i : Nat) : List a -> List (Prod Nat a)
  | [] => []
  | x :: xs => (i, x) :: iterPositions (i + 1) xs

def List.flatten (list : List (List a)) : List a :=
  list
  |> List.foldr List.append []

def findGuard (input : List (List Cell)) : Option Guard :=
  input
  |> iterPositions 0
  |> List.map (fun (y, row) =>
    iterPositions 0 row
    |> List.map (fun (x, cell) => (y, (x, cell)))
  )
  |> List.flatten
  |> List.map (fun (y, (x, cell)) =>
    match cell with
    | Cell.guard dir => some { x := x, y := y, dir := dir : Guard }
    | _ => none
  )
  |> List.find? (Option.isSome)
  |> fun
    | some (some x) => some x
    | _ => none

def convert (input : List (List Cell)) : Option (Prod (Array (Array Bool)) Guard) := do
  let map := List.toArray <| input.map (fun l => List.toArray <| List.map fun
    | Cell.full => true
    | _ => false
  l )
  let guard <- findGuard input
  (map, guard)

partial def loop (visited : List Guard) (map : Array (Array Bool)) (guard : Guard) : List Guard :=
  if visited.contains guard
  then visited
  else match step map guard with
  | none => guard :: visited
  | some next => loop (guard :: visited) map next

def dedup [BEq a] : List a -> List a
  | [] => []
  | [x] => [x]
  | a :: b :: xs =>
    if a == b
    then dedup (a :: xs)
    else a :: dedup (b :: xs)

def dedupPos (input : List Guard) : List (Prod Nat Nat) :=
  input
  |> List.map (fun guard => (guard.x, guard.y))
  |> List.toArray
  |> (fun l => Array.qsort l (fun (x1,y1) (x2,y2) => x1 < x2 || x1 = x2 && y1 < y2))
  |> Array.toList
  |> dedup

def solve (input : List (List Cell)) : Option Nat := do
  let (input, guard) <- convert input.reverse
  let visited := loop [] input guard
  pure <| (dedupPos visited).length

-- Part 2

partial def checkLoop (visited : List Guard) (map : Array (Array Bool)) (guard : Guard) : Bool :=
  if visited.contains guard
  then true
  else match step map guard with
  | none => false
  | some next => checkLoop (guard :: visited) map next

partial def range (min : Nat) (max : Nat) : List Nat :=
  if min >= max
  then []
  else min :: range (min + 1) max

def listRectangle (left : List a) (right : List b) : List (Prod a b) :=
  match left with
  | [] => []
  | a :: left => right.map (a, ·) |> List.append (listRectangle left right)

def allPositions (map : Array (Array a)) : List (Prod Nat Nat) :=
  let x := range 0 (map.getD 0 #[]).size
  let y := range 0 map.size
  listRectangle x y

def setCell (cell : a) (pos : Prod Nat Nat) (map : Array (Array a)) : Array (Array a) :=
  map.getD pos.snd #[]
  |> (fun row => row.setD pos.fst cell)
  |> map.setD pos.snd

def generateMaps (map : Array (Array Bool)) (guard : Guard) : List (Array (Array Bool)) :=
  let candidates := dedupPos <| loop [] map guard
  candidates
  |> List.filter (fun (x, y) => guard.x != x || guard.y != y)
  |> List.map (fun (x, y) => setCell true (x, y) map)

def solve2 (input : List (List Cell)) : Option Nat := do
  let (input, guard) <- convert input.reverse
  generateMaps input guard
  |> List.filter (checkLoop [] · guard)
  |> List.length

-- Parse

def parseCell? : Char -> Option Cell
  | '.' => some Cell.empty
  | '#' => some Cell.full
  | '^' => some (Cell.guard (Direction.up))
  | _ => none

def parseByLine? (input : String) : Option (List Cell) :=
  input.toList
  |> List.mapM parseCell?

def run : IO Unit := Util.run "input/day06.txt" (Util.parseByLine parseByLine?) solve2

instance : ToString Guard where
  toString guard := s!"({guard.x}, {guard.y})"

instance : ToString Cell where
  toString
  | Cell.empty => "."
  | Cell.full => "#"
  | Cell.guard _ => "o"

instance : ToString (List (List Cell)) where
  toString map :=
    map.map (List.map toString)
    |> List.intersperse ["\n"]
    |> List.flatten
    |> List.foldr String.append ""

#eval Util.run "input/day06example.txt" (Util.parseByLine parseByLine?) (fun input => do
  let (input, guard) <- convert input.reverse
  pure <| dedupPos (loop [] input guard)
)

#eval Util.run "input/day06example.txt" (Util.parseByLine parseByLine?) (fun input => do
  let (input, guard) <- convert input.reverse
  let candidates := dedupPos <| loop [] input guard
  pure (
    candidates
    |> List.filter (fun (x, y) => guard.x != x || guard.y != y)
  ).length
)

end Day06
