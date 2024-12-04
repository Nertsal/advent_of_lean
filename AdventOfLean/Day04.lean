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

-- secondary diagonals (down-left / up-right)
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

def parseByLine? (input : String) : Option String := some input

def run : IO Unit := Util.run "input/day04.txt" (Util.parseByLine parseByLine?) solve

#eval Util.run "input/day04example.txt" (Util.parseByLine parseByLine?) (
  fun input => input
  |> List.map String.toList
  |> (fun input => generateCandidates <| input)
  |> List.filter checkCandidate
)

end Day04
