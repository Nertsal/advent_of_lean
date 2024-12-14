import AdventOfLean.Util

namespace Day14

abbrev Position : Type := Prod Int Int

structure Robot where
  position : Position
  velocity : Position
deriving Repr

def addPos (a : Position) (b : Position) : Position :=
  (a.fst + b.fst, a.snd + b.snd)

def wrapCoord (size : Int) (x : Int) : Int :=
  if x < 0
  then x + size
  else if x >= size
    then x - size
    else x

#eval wrapCoord 5 0
#eval wrapCoord 5 4
#eval wrapCoord 5 5
#eval wrapCoord 5 6

def wrapPos (mapSize : Position) (pos : Position) : Position :=
  (wrapCoord mapSize.fst pos.fst, wrapCoord mapSize.snd pos.snd)

def simulateRobot (mapSize : Position) (robot : Robot) : Nat -> Position
  | 0 => robot.position
  | .succ n =>
      let nextPos := wrapPos mapSize (addPos robot.position robot.velocity)
      simulateRobot mapSize { robot with position := nextPos } n

structure QuadrantCounter where
  btmLeft : Nat
  btmRight : Nat
  topLeft : Nat
  topRight : Nat
deriving Repr

def quadZero : QuadrantCounter :=
  {
    btmLeft := 0,
    btmRight := 0,
    topLeft := 0,
    topRight := 0,
  }

def mergeCounters (lhs : QuadrantCounter) (rhs : QuadrantCounter) : QuadrantCounter :=
  {
    btmLeft := lhs.btmLeft + rhs.btmLeft,
    btmRight := lhs.btmRight + rhs.btmRight,
    topLeft := lhs.topLeft + rhs.topLeft,
    topRight := lhs.topRight + rhs.topRight,
  }

def posQuadrant (mapSize : Position) (pos : Position) : QuadrantCounter :=
  let counter := quadZero
  if pos.fst == mapSize.fst / 2
  then counter
  else
  if pos.snd == mapSize.snd / 2
  then counter
  else
  let left := pos.fst < mapSize.fst / 2
  let top := pos.snd < mapSize.snd / 2
  if left
  then if top
    then { counter with topLeft := 1 }
    else { counter with btmLeft := 1 }
  else if top
    then { counter with topRight := 1 }
    else { counter with btmRight := 1 }

def solve (steps : Nat) (mapSize : Position) (input : List Robot) : Nat :=
  input
  |> List.map (fun r => simulateRobot mapSize r steps)
  |> List.map (posQuadrant mapSize)
  |> List.foldr mergeCounters quadZero
  |> (fun counter => counter.btmLeft * counter.btmRight * counter.topLeft * counter.topRight)

-- Parse

def stripPrefix (pref : List Char) (s : List Char) : Option (List Char) :=
  match pref, s with
  | [], [] => some []
  | _, [] => none
  | [], s => some s
  | p :: ps, c :: cs => if p == c
      then stripPrefix ps cs
      else none

#eval stripPrefix "123".toList "12312".toList -- 12
#eval stripPrefix "123".toList "1212321".toList -- none

def findStripPrefix (pref : List Char) (s : List Char) : Option (List Char) :=
  match pref, s with
  | [], [] => some []
  | _, [] => none
  | [], s => some s
  | p :: ps, c :: cs => if p == c
      then match stripPrefix ps cs with
      | .some s => some s
      | .none => findStripPrefix pref cs
      else findStripPrefix pref cs

#eval findStripPrefix "123".toList "12312".toList -- 12
#eval findStripPrefix "123".toList "1212321".toList -- 21

def parseCoord? (input : List Char) : Option Int :=
  input
  |> List.takeWhile Char.isDigit
  |> List.foldl String.push ""
  |> String.toNat?

#eval parseCoord? "123asd".toList

def parsePosition? (input : List Char) : Option Position := do
  let x <- parseCoord? input
  let y <- findStripPrefix ",".toList input
  let y <- parseCoord? y
  pure (x, y)

#eval parsePosition? "1,2asdasd".toList

def parseVelocity? (input : List Char) : Option Position := do
  let x <- match stripPrefix "-".toList input with
  | .none => parseCoord? input
  | .some x => parseCoord? x |> Option.map Int.neg
  let y <- findStripPrefix ",".toList input
  let y <- match stripPrefix "-".toList y with
  | .none => parseCoord? y
  | .some y => parseCoord? y |> Option.map Int.neg
  pure (x, y)

#eval parseVelocity? "1,2asdsad".toList
#eval parseVelocity? "-1,2asdsad".toList
#eval parseVelocity? "-1,-2asdasd".toList
#eval parseVelocity? "1,-2asdsad".toList

def parseLine? (input : String) : Option Robot := do
  let input := input.toList
  let p <- findStripPrefix "p=".toList input
  let v <- findStripPrefix "v=".toList p
  let p <- parsePosition? p
  let v <- parseVelocity? v
  pure { position := p, velocity := v }

#eval parseLine? "p=10,3 v=-1,2"

def run : IO Unit := Util.run "input/day14.txt" (Util.parseByLine parseLine?) (solve 100 (101, 103))

#eval Util.getAnswer "input/day14example.txt" (Util.parseByLine parseLine?) (solve 100 (11, 7))
#eval Util.getAnswer "input/day14example.txt" (Util.parseByLine parseLine?) (fun input =>
  input
  |> List.map (fun r => simulateRobot (11,7) r 100)
  |> List.map (posQuadrant (11,7))
  |> List.foldr mergeCounters quadZero
  -- |> (fun counter => counter.btmLeft * counter.btmRight * counter.topLeft * counter.topRight)
)

end Day14
