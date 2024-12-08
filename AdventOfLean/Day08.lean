import AdventOfLean.Util

namespace Day08

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

def dedupPos (maxX : Int) (maxY : Int) (input : List (Prod Int Int)) : List (Prod Nat Nat) :=
  input
  |> List.filterMap (fun (x, y) =>
    if x < 0 || x > maxX || y < 0 || y > maxY
    then none
    else some (x.toNat, y.toNat)
  )
  |> List.toArray
  |> (fun l => Array.qsort l (fun (x1,y1) (x2,y2) => x1 < x2 || x1 = x2 && y1 < y2))
  |> Array.toList
  |> dedup

def enumerate (i : Nat) : List a -> List (Prod Nat a)
  | [] => []
  | x :: xs => (i, x) :: enumerate (i + 1) xs

def iterPositions (input : List (List a)) : List (Prod (Prod Nat Nat) a) :=
  input
  |> enumerate 0
  |> List.map (fun (y, row) =>
    enumerate 0 row
    |> List.map (fun (x, cell) => ((x, y), cell))
  )
  |> List.flatten

def pairs : List a -> List (Prod a a)
  | [] => []
  | x :: xs => List.append (List.map (x, ·) xs) (pairs xs)

def findAllNodes (input : List (List Char)) : List Char :=
  input
  |> List.flatten
  |> List.filter (fun c => c != '.')
  |> List.toArray
  |> (fun l => Array.qsort l (fun a b => a < b))
  |> Array.toList
  |> dedup

def findNodes (node : Char) (input : List (List Char)) : List (Prod Nat Nat) :=
  iterPositions input
  |> List.filterMap (fun ((x, y), c) =>
    if c = node
    then some (x, y)
    else none
  )

def getAntiNodes (nodeA : Prod Nat Nat) (nodeB : Prod Nat Nat)
  : List (Prod Int Int) :=
  let (x1, y1) := nodeA.map Int.ofNat Int.ofNat
  let (x2, y2) := nodeB.map Int.ofNat Int.ofNat
  let dx := x2 - x1
  let dy := y2 - y1
  let answer := [
    (x1 - dx, y1 - dy),
    (x2 + dx, y2 + dy)
  ]
  if dx.mod 3 = 0 && dy.mod 3 = 0
  then
    let dx3 := dx / 3
    let dy3 := dy / 3
    (x1 + dx3, y1 + dy3)
    :: (x2 - dx3, y2 - dy3)
    :: answer
  else answer

def solve (antinodes : Prod Nat Nat -> Prod Nat Nat -> List (Prod Int Int))
  (input : List (List Char))
  : Nat :=
  findAllNodes input
  |> List.map (fun node =>
    findNodes node input
    |> pairs
    |> List.map (Util.uncurry antinodes)
    |> List.flatten
  )
  |> List.flatten
  |> dedupPos (input.headD []).length.pred input.length.pred
  |> List.length

-- Part 2

-- expects `a > b`
partial def gcdImpl (a : Nat) (b : Nat) : Nat :=
  if b = 0
  then a
  else gcdImpl b (a.mod b)

-- Greatest Common Divisor
def gcd (a : Nat) (b : Nat) : Nat :=
  if a > b
  then gcdImpl a b
  else gcdImpl b a

def normalizeFraction (frac : Prod Nat Nat) : Prod Nat Nat :=
  let (top, btm) := frac
  let d := gcd top btm
  (top / d, btm / d)

#eval normalizeFraction (5, 6)
#eval normalizeFraction (30, 12)

def maxRange : Nat := 50

def range : Nat -> List Nat
  | 0 => [0]
  | .succ n => n :: range n

def getAntiNodes2 (nodeA : Prod Nat Nat) (nodeB : Prod Nat Nat)
  : List (Prod Int Int) :=
  let (x1, y1) := nodeA.map Int.ofNat Int.ofNat
  let (x2, y2) := nodeB.map Int.ofNat Int.ofNat
  let dx := x2 - x1
  let dy := y2 - y1
  let (stepx, stepy) := normalizeFraction (dx.natAbs, dy.natAbs)
  let dx := dx.sign * (Int.ofNat stepx)
  let dy := dy.sign * (Int.ofNat stepy)
  let step := fun i => (x1 + dx * i, y1 + dy * i)
  List.append
    (range maxRange |> List.map Int.ofNat |> List.map step)
    (range maxRange |> List.map Int.negSucc |> List.map step)

-- Parse

def parse? (input : List String) : Option (List (List Char)) :=
  some <| input.map (fun s => s.toList)

def run : IO Unit := Util.run "input/day08.txt" (parse?) (solve getAntiNodes2)

#eval run

end Day08
