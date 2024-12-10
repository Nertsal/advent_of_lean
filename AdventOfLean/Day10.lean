import AdventOfLean.Util

namespace Day10

def List.flatten (list : List (List a)) : List a :=
  list
  |> List.foldr List.append []

def dedup [BEq a] : List a -> List a
  | [] => []
  | [x] => [x]
  | a :: b :: xs =>
    if a == b
    then dedup (a :: xs)
    else a :: dedup (b :: xs)

def dedupPositions (input : List (Prod Nat Nat)) : List (Prod Nat Nat) :=
  input.toArray
  |> (fun l => Array.qsort l (fun (x1, y1) (x2, y2) => x1 < x2 || x1 == x2 && y1 < y2))
  |> Array.toList
  |> dedup

def enumerate (i : Nat) : List a -> List (Prod Nat a)
  | [] => []
  | x :: xs => (i, x) :: enumerate (i + 1) xs

def iterPositions (input : Array (Array a)) : List (Prod (Prod Nat Nat) a) :=
  input.toList
  |> enumerate 0
  |> List.map (fun (y, row) =>
    enumerate 0 row.toList
    |> List.map (fun (x, cell) => ((x, y), cell))
  )
  |> List.flatten

def findTrailHeads (input : Array (Array Nat)) : List (Prod Nat Nat) :=
  input
  |> iterPositions
  |> List.filterMap (fun (pos, h) => if h == 0 then some pos else none)

def neighborDeltas : List (Prod Int Int) :=
  [ (1, 0)
  , (0, 1)
  , (-1, 0)
  , (0, -1)
  ]

def toNat? : Int -> Option Nat
  | .ofNat x => some x
  | .negSucc _ => none

def neighborPositions (pos : Prod Nat Nat) : List (Prod Nat Nat) :=
  neighborDeltas
  |> List.map (fun (dx, dy) => (Int.ofNat pos.fst + dx, Int.ofNat pos.snd + dy))
  |> List.filterMap (fun (x, y) => do
      let x <- toNat? x
      let y <- toNat? y
      pure (x, y)
  )

def findNeighbors (input : Array (Array Nat)) (pos : Prod Nat Nat) : List (Prod Nat Nat) :=
  let maxX := (input.getD 0 #[]).size
  let maxY := input.size
  neighborPositions pos
  |> List.filter (fun (x, y) => x < maxX && y < maxY)

def getHeight (input : Array (Array Nat)) (pos : Prod Nat Nat) : Nat :=
  (input.getD pos.snd #[]).getD pos.fst 0

partial def findTrailsFrom (input : Array (Array Nat)) (currHeight : Nat) (startPos : Prod Nat Nat)
  : List (List (Prod Nat Nat)) :=
  if currHeight = 9
  then [[startPos]]
  else findNeighbors input startPos
    |> List.filter (fun pos => getHeight input pos == currHeight + 1)
    |> List.map (fun pos => findTrailsFrom input (currHeight + 1) pos)
    |> List.flatten

def getSummit : List (Prod Nat Nat) -> Prod Nat Nat
  | [] => panic "empty trail"
  | [x] => x
  | _ :: xs => getSummit xs

def solve (input : Array (Array Nat)) : Nat :=
  input
  |> findTrailHeads
  |> List.map (fun pos =>
      findTrailsFrom input 0 pos
      |> List.map getSummit
      |> dedupPositions
      |> List.length
  )
  |> List.foldr Nat.add 0

-- Parse

def parseDigit? (c : Char) : Option Nat :=
  if c.isDigit
  then some (c.toNat - '0'.toNat)
  else none

def parseLine? (input : String) : Option (Array Nat) := do
  let digits <- input.toList.mapM parseDigit?
  pure digits.toArray

def parse? (input : List String) : Option (Array (Array Nat)) := do
  let lines <-Util.parseByLine parseLine? input
  pure lines.toArray

def run : IO Unit := Util.run "input/day10.txt" parse? solve

#eval Util.run "input/day10example.txt" parse? solve
-- #eval Util.run "input/day10example.txt" parse? (fun input =>
--   findNeighbors input (3, 0)
--   |> List.filter (fun pos => getHeight input pos == 1 + 1)
--   -- |> List.map (fun pos => findTrailsFrom input (0 + 1) pos)
--   -- |> List.flatten
-- )

end Day10
