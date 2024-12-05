import AdventOfLean.Util

namespace Day05

structure Order where
  fst : Nat
  snd : Nat
deriving Repr

def checkOrder (order : Order) (a : Nat) (b : Nat) : Bool :=
  if order.snd = a && order.fst = b
  then false
  else true

def checkPair (orders : List Order) (pair : Prod Nat Nat) : Bool :=
  let (a, b) := pair
  orders.all (fun ord => checkOrder ord a b)

def pairs : List a -> List (Prod a a)
  | [] => []
  | [_] => []
  | a :: tail@(b :: _) => (a, b) :: pairs tail

def check (orders: List Order) (update : List Nat) : Bool :=
  List.all (pairs update) (checkPair orders)

def solve (orders : List Order) (updates: List (List Nat)) : Nat :=
  updates
  |> List.filter (check orders)
  |> List.map (fun l => List.get! l (List.length l / 2))
  |> List.foldr Nat.add 0

-- Part 2

def fix (orders : List Order) : List Nat -> List Nat
  | [] => []
  | [a] => [a]
  | a :: b :: tail =>
    if checkPair orders (a, b)
    then a :: fix orders (b :: tail)
    else b :: fix orders (a :: tail)

partial def fixAll (orders : List Order) (updates : List Nat) : List Nat :=
  let oneFixed := fix orders updates
  if oneFixed = updates
  then updates
  else fixAll orders oneFixed

def solve2 (orders : List Order) (updates: List (List Nat)) : Nat :=
  updates
  |> List.filter (fun upd => not <| check orders upd)
  |> List.map (fixAll orders)
  |> List.map (fun l => List.get! l (List.length l / 2))
  |> List.foldr Nat.add 0

-- Parse

def split : List String -> Prod (List String) (List String)
  | [] => ([], [])
  | "" :: tail => ([], tail)
  | line :: tail =>
    let (lines, tail) := split tail
    (line :: lines, tail)

def parseOrder? (input : String) : Option Order :=
  match input.split (fun c => c = '|') with
  | [fst, snd] => do
    let fst <- fst.toNat?
    let snd <- snd.toNat?
    pure { fst := fst, snd := snd }
  | _ => none

def parseUpdate? (input : String) : Option (List Nat) :=
  input.split (fun c => c = ',')
  |> List.mapM String.toNat?

def parse? (input : List String) : Option (Prod (List Order) (List (List Nat))) := do
  let (left, right) := split input
  let orders <- Util.parseByLine parseOrder? left
  let updates <- Util.parseByLine parseUpdate? right
  pure (orders, updates)

def run : IO Unit := Util.run "input/day05.txt" parse? (Util.uncurry solve2)

end Day05
