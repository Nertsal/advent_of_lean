import AdventOfLean.Util

namespace Day07

structure Equation where
  lhs : Nat
  rhs : List Nat
deriving Repr

inductive Op where
| add : Op
| mul : Op
| cat : Op
deriving Repr

def Op.apply (op : Op) (a : Nat) (b : Nat) : Nat :=
  match op with
  | .add => a + b
  | .mul => a * b
  | .cat => a * 10 ^ (Nat.toDigits 10 b).length + b

def apply (ops : List Op) (args : List Nat) : Nat :=
  match args, ops with
  | [], _ => 0
  | [x], _ => x
  | _, [] => 0
  | a :: b :: tail, op :: ops => apply ops (op.apply a b :: tail)

def check (ops : List Op) (eq : Equation) : Bool :=
  eq.lhs = apply ops eq.rhs

def List.flatten (list : List (List a)) : List a :=
  list
  |> List.foldr List.append []

def generateOps : Nat -> List (List Op)
  | 0 => [[]]
  | .succ n =>
    let tail := generateOps n
    tail.map (fun l => [Op.add :: l, Op.mul :: l])
    |> List.flatten

def solvable (gen : Nat -> List (List Op)) (eq : Equation) : Bool :=
  gen eq.rhs.length.pred
  |> (List.any · (check · eq))

def solve (gen : Nat -> List (List Op)) (input : List Equation) : Nat :=
  input
  |> List.filter (solvable gen)
  |> List.map Equation.lhs
  |> List.foldr Nat.add 0

-- Part 2

def generateOps2 : Nat -> List (List Op)
  | 0 => [[]]
  | .succ n =>
    let tail := generateOps2 n
    tail.map (fun l => [Op.add :: l, Op.mul :: l, Op.cat :: l])
    |> List.flatten

-- Parse

def parseEquation? (line : String) : Option Equation :=
  match line.split (· = ':') with
  | [lhs, rhs] => do
    let lhs <- lhs.toNat?
    let numbers <- rhs.split (· = ' ')
      |> (List.tailD · [])
      |> List.mapM String.toNat?
    pure <| { lhs := lhs, rhs := numbers }
  | _ => none

def run : IO Unit := Util.run "input/day07.txt" (Util.parseByLine parseEquation?) (solve generateOps2)

end Day07
