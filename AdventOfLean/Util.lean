namespace Util

def uncurry (f : a -> b -> c) : Prod a b -> c :=
  fun prod => f prod.fst prod.snd

def parseByLine [Monad m] (parseLine? : String -> m a) : List String -> m (List a) :=
  List.mapM parseLine?

def fileStream (filename : System.FilePath) (mode : IO.FS.Mode) : IO (Option IO.FS.Stream) := do
  let handle ← IO.FS.Handle.mk filename mode
  pure (some (IO.FS.Stream.ofHandle handle))

partial def readLines (stream : IO.FS.Stream) : IO (List String) := do
  let line ← stream.getLine
  if line.isEmpty then
    pure []
  else
    let tail <- readLines stream
    pure (line.trimRight :: tail)

def readFile (filename : System.FilePath) : IO (List String) := do
  let stream <- fileStream filename IO.FS.Mode.read
  let stream := stream.get!
  readLines stream

def writeFile (filename : System.FilePath) (data : String) : IO Unit := do
  let stream <- fileStream filename IO.FS.Mode.write
  let stream := stream.get!
  stream.putStr data

def getAnswer (filename : System.FilePath) (parse? : List String -> Option a) (solve : a -> b)
: IO (Option b) := do
  let input <- Util.readFile filename
  match parse? input with
  | .none => pure none
  | .some input => do
      let answer := solve input
      pure (some answer)

def run [ToString b] (filename : System.FilePath) (parse? : List String -> Option a) (solve : a -> b) : IO Unit := do
  let answer <- getAnswer filename parse? solve
  match answer with
  | .none => pure ()
  | .some a => IO.println a

end Util
