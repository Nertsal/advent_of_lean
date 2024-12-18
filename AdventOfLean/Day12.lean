import AdventOfLean.Util

namespace Day12

abbrev Map := Array (Array Char)

abbrev Position := Prod Nat Nat

structure Region where
  plot : Char
  positions : List Position
deriving Repr

def neighborDeltas : List (Prod Int Int) :=
  [ (1, 0)
  , (0, 1)
  , (-1, 0)
  , (0, -1)
  ]

def neighborPositions (pos : Position) : List Position :=
  neighborDeltas
  |> List.filterMap (fun d =>
      let pos := (Int.ofNat pos.fst + d.fst, Int.ofNat pos.snd + d.snd)
      if pos.fst < 0 || pos.snd < 0
      then none
      else some (pos.fst.natAbs, pos.snd.natAbs)
  )

#eval neighborPositions (0,0)
#eval neighborPositions (1,4)

def isNeighbor (a : Position) (b : Position) : Bool :=
  let d := (Int.ofNat a.fst - Int.ofNat b.fst, Int.ofNat a.snd - Int.ofNat b.snd)
  d.fst.natAbs + d.snd.natAbs == 1

#eval isNeighbor (0,0) (0,1)
#eval isNeighbor (0,0) (1,1)

def regionArea (input : Region) : Nat :=
  input.positions.length

def countNeighbors (input : Region) (pos : Position) : Nat :=
  input.positions
  |> List.filter (isNeighbor pos)
  |> List.length

def countWalls (input : Region) (pos : Position) : Nat :=
  4 - countNeighbors input pos

def regionPerimeter (input : Region) : Nat :=
  input.positions
  |> List.map (countWalls input)
  |> List.foldr Nat.add 0

def regionPrice (input : Region) : Nat :=
  regionArea input * regionPerimeter input

def range : Nat -> List Nat
  | 0 => []
  | .succ n => n :: range n

#eval range 3

def List.flatten (list : List (List a)) : List a :=
  list
  |> List.foldr List.append []

def allPositions (input : Map) : List Position :=
  range input.size
  |> List.map (fun y =>
      range (input.getD 0 #[]).size
      |> List.map (fun x => (x, y))
  )
  |> List.flatten

structure FindState where
  map : Map
  checked : List Position
  regions : List Region
deriving Repr

def getPlotM (pos : Position) : StateM FindState Char := do
  let state <- StateT.get
  pure <| (state.map.get! pos.snd).get! pos.fst

def getNeighborsM (pos : Position) : StateM FindState (List (Prod Position Char)) := do
  let state <- StateT.get
  pure (neighborPositions pos
    |> List.filterMap (fun pos => do
        let row <- state.map.get? pos.snd
        let plot <- row.get? pos.fst
        pure (pos, plot)
    )
  )

def enumerate (i : Nat) : List a -> List (Prod Nat a)
  | [] => []
  | x :: xs => (i, x) :: enumerate (i + 1) xs

def findRegionM (pos : Position) : StateM FindState (Option Nat) := do
  let state <- StateT.get
  let plot <- getPlotM pos
  let neighbors <- getNeighborsM pos
  let i : Option Nat := neighbors
    |> List.filter (fun (_, c) => plot == c)
    |> List.findSome? (fun (pos, _) =>
        state.regions
        |> enumerate 0
        |> List.findSome? (fun (i, r) =>
            if List.contains r.positions pos
            then some i
            else none
        )
    )
  pure i

def markCheckedM (pos : Position) : StateM FindState Bool := do
  let state <- StateT.get
  let checked := state.checked.contains pos
  if not checked
  then do
    StateT.set { state with checked := pos :: state.checked }
    pure checked
  else pure checked

def newRegionM (pos : Position) : StateM FindState Unit := do
  let state <- StateT.get
  let plot <- getPlotM pos
  let newRegion := { plot := plot, positions := [pos] }
  StateT.set { state with regions := newRegion :: state.regions }

def update (f : a -> a) : Nat -> List a -> List a
  | _, [] => []
  | 0, x :: xs => f x :: xs
  | .succ n, x :: xs => x :: update f n xs

def addToRegionM (i : Nat) (pos : Position) : StateM FindState Unit := do
  let state <- StateT.get
  let newRegions := update
    (fun r => { r with positions := pos :: r.positions : Region })
    i state.regions
  StateT.set { state with regions := newRegions }

partial def processPositionM (pos : Position) : StateM FindState Unit := do
  let wasChecked <- markCheckedM pos
  if wasChecked
  then pure ()
  else
  let regionI <- findRegionM pos
  match regionI with
  | .none => do
      newRegionM pos
  | .some i => addToRegionM i pos
  -- recursively add all neighbors
  let plot <- getPlotM pos
  let neighbors <- getNeighborsM pos
  neighbors
  |> List.filterMap (fun (pos, c) =>
      if c == plot
      then some pos
      else none
  )
  |> (fun l => List.forM l processPositionM)

def findRegionsM : StateM FindState Unit := do
  let state <- StateT.get
  allPositions state.map
  |> (fun l => List.forM l processPositionM)

def findRegions (input : Map) : List Region := Id.run do
  let state := { map := input, checked := [], regions := [] : FindState }
  let ((), result) <- StateT.run findRegionsM state
  pure (result.regions)

def solve (costF : Region -> Nat) (input : Map) : Nat :=
  input
  |> findRegions
  |> List.map costF
  |> List.foldr Nat.add 0

-- Part 2

structure CostState where
  region : Region
  checked : List Position
  sides : Nat
deriving Repr

def markCheckedCostM (pos : Position) : StateM CostState Bool := do
  let state <- StateT.get
  let checked := state.checked.contains pos
  if not checked
  then do
    StateT.set { state with checked := pos :: state.checked }
    pure checked
  else pure checked

def rotate (dir : Prod Int Int) : Prod Int Int :=
  (dir.snd, -dir.fst)

def negate (dir : Prod Int Int) : Prod Int Int :=
  (-dir.fst, -dir.snd)

def addM (pos : Position) (dir : Prod Int Int) : StateM CostState (Option Position) :=
  let pos := (Int.ofNat pos.fst + dir.fst, Int.ofNat pos.snd + dir.snd)
  if pos.fst < 0 || pos.snd < 0
  then pure none
  else do
    let pos := pos.map Int.natAbs Int.natAbs
    let state <- StateT.get
    pure <| if state.region.positions.contains pos
      then some pos
      else none
  
def checkDir (pos : Position) (dir : Prod Int Int) : StateM CostState Bool := do
  let pos <- addM pos dir
  pure pos.isSome

def incSidesM : StateM CostState Unit := do
  let state <- StateT.get
  StateT.set { state with sides := state.sides + 1 }

partial def markAllCheckedIn (pos : Position) (dir : Prod Int Int) (moveDir : Prod Int Int)
: StateM CostState Unit
:= do
  let check <- checkDir pos dir
  if check
  then pure ()
  else
  let _ <- markCheckedCostM pos
  let next <- addM pos moveDir
  match next with
  | .none => pure ()
  | .some pos => markAllCheckedIn pos dir moveDir

def checkPositionSide (dir : Prod Int Int) (pos : Position) : StateM CostState Unit := do
  let wasChecked <- markCheckedCostM pos
  let noSide <- checkDir pos dir
  if wasChecked || noSide
  then pure ()
  else
    incSidesM
    let moveDir := rotate dir
    markAllCheckedIn pos dir moveDir
    markAllCheckedIn pos dir (negate moveDir)

def regionSidesM (dir : Prod Int Int) : StateM CostState Unit := do
  let state <- StateT.get
  state.region.positions
  |> (fun l => List.forM l (checkPositionSide dir))

def regionSides (input : Region) : Nat := Id.run do
  let state := { region := input, checked := [], sides := 0 : CostState }
  let ((), up) <- StateT.run (regionSidesM (0,1)) state
  let ((), down) <- StateT.run (regionSidesM (0,-1)) state
  let ((), left) <- StateT.run (regionSidesM (-1,0)) state
  let ((), right) <- StateT.run (regionSidesM (1,0)) state
  pure (up.sides + down.sides + left.sides + right.sides)

def regionPrice2 (input : Region) : Nat :=
  regionArea input * regionSides input

-- Parse

def parse? (input : List String) : Option Map :=
  pure (input
    |> List.map (fun s => s.toList.toArray)
    |> List.toArray
  )

def run : IO Unit := Util.run "input/day12.txt" parse? (solve regionPrice2)

#eval Util.getAnswer "input/day12example2.txt" parse? (solve regionPrice) -- 140
#eval Util.getAnswer "input/day12example3.txt" parse? (solve regionPrice) -- 772
#eval Util.getAnswer "input/day12example.txt" parse? (solve regionPrice) -- 1930

#eval Util.getAnswer "input/day12example2.txt" parse? (solve regionPrice2) -- 80
#eval Util.getAnswer "input/day12example3.txt" parse? (solve regionPrice2) -- 436
#eval Util.getAnswer "input/day12example.txt" parse? (solve regionPrice2) -- 1206

end Day12
