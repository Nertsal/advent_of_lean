import AdventOfLean.Util

namespace Day09

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

-- Part 2

structure Block where
  id : Nat
  length : Nat
  emptyPad : Nat
deriving Repr, BEq

def convert2 : List Nat -> List Block
  | [] => []
  | [x] => [{ id := 0, length := x, emptyPad := 0 }]
  | a :: b :: tail =>
      { id := 0, length := a, emptyPad := b }
      :: List.map (fun b => { b with id := b.id + 1 }) (convert2 tail)

def remove : Nat -> List a -> Prod (Option a) (List a)
  | _, [] => (none, [])
  | 0, x :: xs => (some x, xs)
  | .succ n, x :: xs =>
      let (elem, tail) := remove n xs
      (elem, x :: tail)

def insert (elem : a) : Nat -> List a -> List a
  | 0, input => elem :: input
  | .succ _, [] => [] -- error
  | .succ n, x :: xs => x :: insert elem n xs

def replace (elem : a) : Nat -> List a -> List a
  | _, [] => [] -- error
  | 0, _ :: xs => elem :: xs
  | .succ n, x :: xs => x :: replace elem n xs

def removeBlock (i : Nat) (input : List Block) : Option (List Block) :=
  if i = 0
  then do
    let (removeBlock, removed) := remove 0 input
    let removeBlock <- removeBlock
    pure <| { removeBlock with length := 0, emptyPad := removeBlock.length + removeBlock.emptyPad }
      :: removed
  else do
    let emptyBlock <- input.get? i.pred
    let (removeBlock, removed) := remove i input
    let removeBlock <- removeBlock
    pure <| replace
      { emptyBlock with emptyPad := emptyBlock.emptyPad + removeBlock.length + removeBlock.emptyPad }
      i.pred removed

#eval removeBlock 1 (convert2 [1,2,3,4,5])
#eval removeBlock 0 (convert2 [1,2,3,4,5])

def fitNext (lastI : Nat) (input : List Block) : Option (Prod Nat (List Block)) := do
  let (emptyI, moveI, moveBlock) <- input
    |> enumerate 0
    |> List.filter (fun (_, block) => block.id < lastI)
    |> List.reverse
    |> List.findSome? (fun (moveI, moveBlock) =>
      enumerate 0 input
        |> List.take moveI
        |> List.find? (fun (_, block) => block.emptyPad >= moveBlock.length)
        |> Option.map (fun (emptyI, _emptyBlock) => (emptyI, moveI, moveBlock))
    )
  let result <- input |> removeBlock moveI
  let emptyBlock <- result.get? emptyI
  let result := result
  |> insert { moveBlock with emptyPad := emptyBlock.emptyPad - moveBlock.length } emptyI.succ
  |> replace { emptyBlock with emptyPad := 0 } emptyI
  pure (moveBlock.id, result)

def test (input : List Block) (emptyI : Nat) (emptyBlock : Block) : Option (Nat × Block × Nat × Block) :=
  enumerate 0 input
    |> List.drop emptyI.succ
    |> List.reverse
    |> List.find? (fun (_, block) => block.length <= emptyBlock.emptyPad)
    |> Option.map (fun (moveI, moveBlock) => (emptyI, emptyBlock, moveI, moveBlock))

#eval removeBlock 1 (convert2 [1,3,3,3,5,0])
#eval fitNext 999999 (convert2 [1,3,3,3,5,0])
#eval fitNext 2 (convert2 [0,0,3,1,1,9,5,3])

#eval (do
  let input := convert2 [0,4,1,3,3,3,5,3]
  let (i, input) <- fitNext 9999 input
  let (i, input) <- fitNext i input
  pure input
)

partial def compact2 (lastI : Nat) (input : List Block) : List Block :=
  match fitNext lastI input with
  | .none => input
  | .some (newI, newInput) => if input == newInput
      then input
      else compact2 newI newInput

#eval compact2 9999 (convert2 [1,2,3,4,1])
#eval compact2 9999 (convert2 [0,4,1,3,3,3,5,3])

def convertToNats : List Block -> List (Option Nat)
  | [] => []
  | b :: bs => repeatElem (some b.id) b.length
      ++ repeatElem none b.emptyPad
      ++ convertToNats bs

def convertToCheck (input : List (Option Nat)) : List (Prod Nat Nat) :=
  input
  |> enumerate 0
  |> List.filterMap (fun (i, x) => x.map (i, ·))

def checkSum2 (input : List Block) : Nat :=
  input |> convertToNats |> convertToCheck |> checkSum

def solve2 (input : List Nat) : Nat :=
  let blocks := input |> convert2
  blocks |> compact2 blocks.length |> checkSum2

-- Parse

def parseDigit? (c : Char) : Option Nat :=
  if c.isDigit
  then some <| c.toNat - '0'.toNat
  else none

def parse? (input : List String) : Option (List Nat) := do
  let line <- input.head?
  line.toList.mapM parseDigit?

def run : IO Unit := Util.run "input/day09.txt" parse? solve2
#eval Util.run "input/day09example.txt" parse? solve2

end Day09
