import AdventOfLean.Util
import Lean.Data.HashMap

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

def solve (n : Nat) (input : List Nat) : Nat :=
  input
  |> repeatApply n blink
  |> List.length

-- Part 2
-- backup/day11.rs

structure CacheM (key : Type) (value : Type) (a : Type) [BEq key] [Hashable key] where
  runCached : Lean.HashMap key value -> Prod (Lean.HashMap key value) a

abbrev StoneCacheM (a : Type) : Type := CacheM (Prod Nat Nat) Nat a

-- instance (key : Type) (value : Type) [BEq key] [Hashable key]
--   : Monad (fun a => CacheM key value a) where
--   pure a := { runCached := fun cache => (cache, a) }
--   bind result next := {
--     runCached := (fun cache1 =>
--       let (cache2, a) := result.runCached cache1
--       (next a).runCached cache2
--     )
--   }

instance : Monad StoneCacheM where
  pure a := { runCached := fun cache => (cache, a) }
  bind result next := {
    runCached := (fun cache1 =>
      let (cache2, a) := result.runCached cache1
      (next a).runCached cache2
    )
  }

def CacheM.get (key : k) [BEq k] [Hashable k] : CacheM k v (Option v) :=
  {
    runCached := (fun cache =>
      (cache, cache.find? key)
    )
  }

def CacheM.insert (key : k) (value : v) [BEq k] [Hashable k] : CacheM k v Unit :=
  {
    runCached := (fun cache =>
      (cache.insert key value, ())
    )
  }

def blinkStone2Raw : Nat -> Nat -> StoneCacheM Nat
  | 0, _ => pure 1
  | .succ n, 0 => blinkStone2Raw n 1
  | .succ n, stone =>
      let digits := Nat.toDigits 10 stone
      if digits.length % 2 == 0
      then do
        let (a, b) := split (digits.length / 2) digits
        let left <- blinkStone2Raw n (digitsToNat a)
        let right <- blinkStone2Raw n (digitsToNat b)
        pure <| left + right
      else
        blinkStone2Raw n (stone * 2024)

def blinkStone2 (n : Nat) (stone : Nat) : StoneCacheM Nat := do
  let cached <- CacheM.get (n, stone)
  match cached with
  | .some count => pure count
  | .none =>
      let count <- blinkStone2Raw n stone
      CacheM.insert (n, stone) count
      pure count

def solve2 (n : Nat) (input : List Nat) : Nat :=
  input
  |> List.mapM (blinkStone2 n)
  |> (fun m => CacheM.runCached m Lean.HashMap.empty)
  |> Prod.snd
  |> List.foldr Nat.add 0

-- Parse

def parse? : List String -> Option (List Nat)
  | [line] => do
      line.split (· == ' ')
      |> List.mapM String.toNat?
  | _ => none

def run : IO Unit := Util.run "input/day11.txt" parse? (solve2 75)

#eval Util.run "input/day11example.txt" parse? (solve 25)
#eval Util.run "input/day11example.txt" parse? (solve2 25)

end Day11
