import AdventOfLean.Util

namespace Day13

abbrev Position : Type := Prod Int Int

structure Machine where
  buttonA : Position
  buttonB : Position
  prize : Position
deriving Repr

def solveMachineImpl (input : Machine) : Option (Prod Int Int) := do
  let (x1,y1) := input.buttonA
  let (x2,y2) := input.buttonB
  let (x,y) := input.prize

  let aNum := y * x2 - x * y2
  let aDen := x2 * y1 - x1 * y2
  let a <- if aNum % aDen != 0
    then none
    else aNum / aDen

  let bNum := x - a * x1
  let bDen := x2
  let b <- if bNum % bDen != 0
    then none
    else bNum / bDen

  pure (a, b)

def solveMachine (input : Machine) : Option Nat := do
  let (a, b) <- solveMachineImpl input
  if a < 0 || b < 0
  then none
  else (a * 3 + b).natAbs

def solve (input : List Machine) : Nat :=
  input
  |> List.filterMap solveMachine
  |> List.foldr Nat.add 0

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

def parseButton? (input : String) : Option Position := do
  let x <- findStripPrefix "X+".toList input.toList
  let x <- parseCoord? x
  let y <- findStripPrefix "Y+".toList input.toList
  let y <- parseCoord? y
  pure (x, y)

#eval parseButton? "Button A: X+94, Y+34"
#eval parseButton? "Button B: X+22, Y+67"

def parsePrize? (input : String) : Option Position := do
  let x <- findStripPrefix "X=".toList input.toList
  let x <- parseCoord? x
  let y <- findStripPrefix "Y=".toList input.toList
  let y <- parseCoord? y
  pure (x, y)

#eval parsePrize? "Prize: X=8400, Y=5400"

def parseMachine? (input : List String) : Option Machine :=
  match input with
  | [a, b, prize] => do
      let a <- parseButton? a
      let b <- parseButton? b
      let prize <- parsePrize? prize
      pure { buttonA := a, buttonB := b, prize := prize }
  | _ => none

#eval parseMachine? ["Button A: X+26, Y+66", "Button B: X+67, Y+21", "Prize: X=12748, Y=12176"]

def List.split [BEq a] (sep : a) : List a -> List (List a)
  | [] => []
  | x :: xs => if sep == x
      then [] :: split sep xs
      else match split sep xs with
        | [] => [[x]]
        | ys :: yss => (x :: ys) :: yss

#eval List.split 0 [1,2,0,1,0,2,3,0,2]

def parse? (input : List String) : Option (List Machine) :=
  input
  |> List.split ""
  |> List.mapM parseMachine?

def run : IO Unit := Util.run "input/day13.txt" parse? solve

#eval Util.run "input/day13example.txt" parse? solve
#eval Util.run "input/day13example.txt" parse? (fun input =>
  input
  -- |> List.length
  |> List.map solveMachineImpl
)

end Day13
