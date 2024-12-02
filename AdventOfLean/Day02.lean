import AdventOfLean.Util

namespace Day02

-- Logic

def checkProp (op : Nat -> Nat -> Bool) (input : List Nat) : Bool :=
  match input.tail? with
    | none => true
    | some tail =>
      List.zip input tail
      |> List.foldr (fun (a, b) acc => acc && op a b) true

def distance (a : Nat) (b : Nat) : Nat :=
  if a < b
  then b - a
  else a - b

def deltaProp (a : Nat) (b : Nat) : Bool :=
  let d := distance a b
  1 <= d && d <= 3

def isSafe (input : List Nat) : Bool :=
  checkProp deltaProp input
  && (checkProp (· < ·) input || checkProp (· > ·) input)

def solve (input : List (List Nat)) : Nat :=
  input
  |> List.filter isSafe
  |> List.length

-- Parsing

def parseLine? (input : String) : Option (List Nat) :=
  input.split Char.isWhitespace
  |> List.mapM String.toNat?

def run : IO Unit := Util.run "input/day02.txt" (Util.parseByLine parseLine?) solve

end Day02
