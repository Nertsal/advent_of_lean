import AdventOfLean.Util

namespace Day09

structure Block where
  id : Nat
  length : Nat
  emptyPad : Nat
deriving Repr

def repeatElem (elem : a) : Nat -> List a
  | 0 => []
  | .succ n => elem :: repeatElem elem n

def enumerate (i : Nat) : List a -> List (Prod Nat a)
  | [] => []
  | x :: xs => (i, x) :: enumerate (i + 1) xs

def convert : List Nat -> List (Option Nat)
  | [] => []
  | [x] => repeatElem (some 0) x
  | a :: b :: tail =>
      repeatElem (some 0) a
      ++ repeatElem none b
      ++ List.map (fun x => x.map Nat.succ) (convert tail)

def compact (input : List (Option Nat)) : List (Prod Nat Nat) :=
  let totalLength := input.filter Option.isSome |> List.length
  let emptyPositions := enumerate 0 input
    |> List.filterMap (
      fun (i, x) => match x with
      | .none => some i
      | .some _ => none
    )
  let inPlace := enumerate 0 input
    |> List.take totalLength
    |> List.filterMap (fun (i, x) => x.map (i, ·))
  let moved := input
    |> List.drop totalLength
    |> List.filterMap id
    |> List.reverse
    |> List.zip emptyPositions
  inPlace ++ moved

def checkSum (input : List (Prod Nat Nat)) : Nat :=
  input
  |> List.foldr (fun (pos, id) acc => acc + pos * id) 0

def solve (input : List Nat) : Nat :=
  input |> convert |> compact |> checkSum

-- Parse

def parseDigit? (c : Char) : Option Nat :=
  if c.isDigit
  then some <| c.toNat - '0'.toNat
  else none

def parse? (input : List String) : Option (List Nat) := do
  let line <- input.head?
  line.toList.mapM parseDigit?

def run : IO Unit := Util.run "input/day09.txt" parse? solve

end Day09
