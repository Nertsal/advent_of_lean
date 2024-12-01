import AdventOfLean.Util

namespace Day01

-- Logic

def distance : Prod Nat Nat -> Nat
  | (a, b) => if a < b
    then b - a
    else a - b

def split : List (Prod Nat Nat) -> Prod (List Nat) (List Nat)
  | [] => ([], [])
  | x :: xs =>
    let tail := split xs
    (
      x.fst :: tail.fst,
      x.snd :: tail.snd
    )

def solve (input : List (Prod Nat Nat)) : Nat :=
  let f : List Nat -> Array Nat := fun l => Array.qsort (List.toArray l) (fun a b => a < b)
  split input
  |> Prod.map f f
  |> Util.uncurry Array.zip
  |> Array.map distance
  |> Array.foldr Nat.add 0

-- Parsing

def parseLine? (input : String) : Option (Prod Nat Nat) :=
  match String.split input Char.isWhitespace with
  | [fst, "", "", snd] => do
    let fst <- fst.toNat?
    let snd <- snd.toNat?
    return (fst, snd)
  | _ => Option.none

def parse? (input : List String) : Option (List (Prod Nat Nat)) :=
  input
  |> List.mapM parseLine?

def run : IO Unit := Util.run "input/day01.txt" parse? solve

end Day01
