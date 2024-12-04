import AdventOfLean.Util

namespace Day04

def uncurry (f : a -> b -> c) : Prod a b -> c := fun (a, b) => f a b

def List.flatten (list : List (List a)) : List a :=
  list
  |> List.foldr List.append []

def List.last? : List a -> Option a
  | [] => none
  | _ :: xs => List.last? xs

def checkCandidate (input : List Char) : Bool :=
  input = "XMAS".toList || input = "SAMX".toList

def mirror : List (List a) -> List (List a) := List.map List.reverse

def transpose : List (List a) -> List (List a)
  | [] => []
  | [row] => row.map (fun x => [x])
  | x :: xs => transpose xs |> List.zip x |> List.map (uncurry List.cons)

def windows (windowSize : Nat) (input : List a) : List (List a) :=
  if windowSize == 0
  then []
  else match input with
    | [] => []
    | x :: xs =>
      let head :=
        x :: xs.take windowSize.pred
        |> some
        |> Option.filter (List.length · = windowSize)
      let tail := windows windowSize xs
      match head with
        | none => tail
        | some head => head :: tail

def sampleLength : Nat := 4

def generateHorizontal (input : List (List a)) : List (List a) :=
  input
  |> List.map (windows sampleLength)
  |> List.flatten

def mergeDiagonal (row : List a) (diagonals : List (List a)) : List (List a) :=
  match row, diagonals with
  | [], [] => []
  | [], diags => diags
  | row, [] => row.map (fun x => [x])
  | x :: xs, d :: ds => (x :: d) :: mergeDiagonal xs ds

def expandDiagonal (row : List a) (diagonals : List (List a)) : List (List a) :=
  match row with
  | [] => []
  | x :: xs => [x] :: mergeDiagonal xs diagonals

def diagonals : List (List a) -> List (List a)
  | [] => []
  | x :: xs => diagonals xs |> expandDiagonal x

-- main diagonals (up-left / down-right)
def generateDiagonalLeft (input : List (List a)) : List (List a) :=
  diagonals input |> generateHorizontal

def generateCandidates (input : List (List a)) : List (List a) :=
  generateHorizontal input
  |> List.append (generateHorizontal <| transpose input)
  |> List.append (generateDiagonalLeft input)
  |> List.append (generateDiagonalLeft <| mirror input)

def solve (input : List String) : Nat :=
  input
  |> List.map String.toList
  |> generateCandidates
  |> List.filter checkCandidate
  |> List.length

-- Part 2

partial def leftDiagonal : List (List a) -> List a
  | (x :: _) :: rows => x :: leftDiagonal (rows.map (fun xs => xs.tailD []))
  | _ => []

partial def rightDiagonal : List (List a) -> List a := (leftDiagonal <| mirror ·)

def checkXMas (input : List (List Char)) : Bool :=
  let leftD := leftDiagonal input
  let left := leftD = "MAS".toList || leftD = "SAM".toList
  let rightD := rightDiagonal input
  let right := rightD = "MAS".toList || rightD = "SAM".toList
  left && right

def generateCellsAt (row : List a) (rows : List (List a)) : List (List (List a)) :=
  match row with
  | a1 :: tail@(b1 :: c1 :: _) =>
    let tail := generateCellsAt tail (rows.map fun x => x.tailD [])
    match rows with
      | (a2 :: b2 :: c2 :: _)
        :: (a3 :: b3 :: c3 :: _)
        :: _ => [[a1,b1,c1],[a2,b2,c2],[a3,b3,c3]] :: tail
      | _ => []
  | _ => []

def generateCells : List (List a) -> List (List (List a))
  | [] => []
  | row :: rows => generateCellsAt row rows |> List.append (generateCells rows)

def solve2 (input : List String) : Nat :=
  input
  |> List.map String.toList
  |> generateCells
  |> List.filter checkXMas
  |> List.length

def parseByLine? (input : String) : Option String := some input

def run : IO Unit := Util.run "input/day04.txt" (Util.parseByLine parseByLine?) solve2

end Day04
