namespace Util

def uncurry (f : a -> b -> c) : Prod a b -> c :=
  fun prod => f prod.fst prod.snd

def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let fileExists ← filename.pathExists
  if not fileExists then
    let stderr ← IO.getStderr
    stderr.putStrLn s!"File not found: {filename}"
    pure none
  else
    let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle handle))

partial def readLines (stream : IO.FS.Stream) : IO (List String) := do
  let line ← stream.getLine
  if line.isEmpty then
    pure []
  else
    let tail <- readLines stream
    pure (line.trimRight :: tail)

def readFile (filename : System.FilePath) : IO (List String) := do
  let stream <- fileStream filename
  let stream := stream.get!
  readLines stream

def run [Inhabited a] [ToString b] (filename : System.FilePath) (parse? : List String -> Option a) (solve : a -> b) : IO Unit := do
  let input <- Util.readFile filename
  let input := (parse? input).get!
  let answer := solve input
  IO.println answer

end Util
