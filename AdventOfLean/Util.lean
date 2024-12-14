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

def getAnswer [Inhabited a] (filename : System.FilePath) (parse? : List String -> Option a) (solve : a -> b) : IO b := do
  let input <- Util.readFile filename
  let input := (parse? input).get!
  let answer := solve input
  pure answer

def run [Inhabited a] [ToString b] (filename : System.FilePath) (parse? : List String -> Option a) (solve : a -> b) : IO Unit := do
  let answer <- getAnswer filename parse? solve
  IO.println answer

end Util
