import AdventOfLean.Util

namespace Day03

structure Mul where
  lhs : Nat
  rhs : Nat
deriving Repr

inductive Op where
| do : Op
| dont : Op
| mul (m : Mul) : Op
deriving Repr

def Mul.apply (op : Mul) : Nat :=
  op.lhs * op.rhs

def solve (input : List Mul) : Nat :=
  input
  |> List.map Mul.apply
  |> List.foldr Nat.add 0

def solve2 (activated : Bool) : List Op -> Nat
| [] => 0
| (op :: ops) => match op with
  | Op.do => solve2 true ops
  | Op.dont => solve2 false ops
  | Op.mul m =>
    let v := if activated then m.apply else 0
    v + solve2 activated ops

def List.flatten (list : List (List a)) : List a :=
  list
  |> List.foldr List.append []

structure Parsed (a : Type) where
  data : Option a
  tail : List Char
deriving Repr

instance : Functor Parsed where
  map f parsed := { data := Functor.map f parsed.data, tail := parsed.tail }

def Parsed.empty : Parsed a :=
  { data := none, tail := [] }

def Parser (a : Type) := List Char -> Parsed a

instance : Functor Parser where
  map f parser := fun s => Functor.map f (parser s)

instance : Applicative Parser where
  pure a := fun s => { data := some a, tail := s }
  seq f parser := fun s =>
    let parsedf := f s
    let parseda := (parser ()) parsedf.tail
    match parsedf.data with
      | none => { data := none, tail := parseda.tail }
      | some f => { data := Functor.map f parseda.data, tail := parseda.tail }

instance : Monad Parser where
  pure a := fun s => { data := some a, tail := s }
  bind ma f := fun s =>
    let parseda := ma s
    match parseda.data with
    | none => { data := none, tail := parseda.tail }
    | some a => (f a) parseda.tail

instance : Alternative Parser where
  failure := fun s => { data := none, tail := s }
  orElse left right := fun s =>
    let parsedLeft := left s
    match parsedLeft.data with
      | none => (right ()) s
      | some _ => parsedLeft

def Parser.filter (cond : a -> Bool) (parser : Parser a) : Parser a :=
  fun s =>
    let parseda := parser s
    { parseda with data := Option.filter cond parseda.data }

def Parser.something (data : Option a) : Parser a :=
  fun s => { data := data, tail := s }

def Parser.front : Parser Char
  | [] => Parsed.empty
  | c :: cs => { data := c, tail := cs }

def Parser.one (cond : Char -> Bool) : Parser Char :=
  Parser.filter cond Parser.front

partial def Parser.many (parser : Parser a) : Parser (List a) := fun s =>
  let parseda := parser s
  match parseda.data with
  | none => { data := some [], tail := s }
  | some a => parseda.tail |> (do
      let as <- Parser.many parser
      pure <| a :: as
    )

def Parser.many1 (parser : Parser a) : Parser (List a) := do
  let a <- parser
  let as <- Parser.many parser
  pure <| a :: as 

def Parser.while (cond : Char -> Bool) : Parser String :=
  Functor.map String.mk <| Parser.many <| Parser.one cond

def Parser.exactL (ref : List Char) : Parser (List Char) := fun s =>
  match ref with
  | [] => { data := some [], tail := s }
  | (head :: tail) =>
    match s with
    | [] => Parsed.empty
    | (c :: cs) => if c == head
      then Parser.exactL tail cs |> Functor.map (c :: ·)
      else { data := none, tail := s }

def Parser.exact (s : String) : Parser String :=
  Parser.exactL (String.toList s) |> Functor.map String.mk

def parse3Digit : Parser Nat := do
  let digits <- Parser.filter (String.length · <= 3) <| Parser.while Char.isDigit
  Parser.something digits.toNat?

def parseMul : Parser Mul := do
  let _ <- Parser.exact "mul("
  let lhs <- parse3Digit
  let _ <- Parser.exact ","
  let rhs <- parse3Digit
  let _ <- Parser.exact ")"
  pure <| { lhs := lhs, rhs := rhs }

def parseDo : Parser Op := do
  let _ <- Parser.exact "do()"
  pure Op.do

def parseDont : Parser Op := do
  let _ <- Parser.exact "don't()"
  pure Op.dont

def parseOp : Parser Op :=
  parseDo <|> parseDont <|> (Functor.map Op.mul parseMul)

def subStrsL (input : List Char) : List (List Char) :=
  let tail := match input with
    | [] => []
    | (_ :: cs) => subStrsL cs
  input :: tail

def parseLine? (input : String) : Option (List Op) :=
  subStrsL (String.toList input)
  |> List.filterMap (fun s => (parseOp s).data)
  |> some

def parse? (input : List String) : Option (List Op) :=
  input
  |> List.mapM parseLine?
  |> Option.map List.flatten

def run : IO Unit := Util.run "input/day03.txt" parse? (solve2 true)

end Day03
