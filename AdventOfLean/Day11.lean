import AdventOfLean.Util

namespace Day11

def digitsToNat : List Char -> Nat
  | [] => 0
  | x :: xs => (x.toNat - '0'.toNat) * 10 ^ xs.length + digitsToNat xs

#eval digitsToNat (Nat.toDigits 10 123)
#eval digitsToNat (Nat.toDigits 10 1)

def split : Nat -> List a -> Prod (List a) (List a)
  | _, [] => ([], [])
  | 0, xs => ([], xs)
  | .succ n, x :: xs =>
    let (head, tail) := split n xs
    (x :: head, tail)

#eval split 0 [1,2,3,4]
#eval split 1 [1,2,3,4]

def blinkStone (stone : Nat) : List Nat :=
  if stone == 0
  then [1]
  else
    let digits := Nat.toDigits 10 stone
    if digits.length % 2 == 0
    then
      let (a, b) := split (digits.length / 2) digits
      [digitsToNat a, digitsToNat b]
    else
      [stone * 2024]

#eval blinkStone 0
#eval blinkStone 1
#eval blinkStone 10

def List.flatten (list : List (List a)) : List a :=
  list
  |> List.foldr List.append []

def blink (stones : List Nat) : List Nat :=
  stones
  |> List.map blinkStone
  |> List.flatten

#eval blink [125, 17]

def repeatApply (n : Nat) (f : a -> a) (input : a) : a :=
  match n with
  | 0 => input
  | .succ n => repeatApply n f (f input)

def solve (input : List Nat) : Nat :=
  input
  |> repeatApply 25 blink
  |> List.length

-- Parse

def parse? : List String -> Option (List Nat)
  | [line] => do
      line.split (· == ' ')
      |> List.mapM String.toNat?
  | _ => none

def run : IO Unit := Util.run "input/day11.txt" parse? solve

#eval Util.run "input/day11example.txt" parse? solve

end Day11
