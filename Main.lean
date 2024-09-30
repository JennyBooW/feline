import Feline

-- The dump function is declared partial, because it calls itself recursively on input
-- that is not immediately smaller than an argument. When a function is declared to be
-- partial, Lean does not require a proof that it terminates.

def buffer_size : USize := 20 * 1024

-- read from the stream.

partial def dump (stream : IO.FS.Stream) : IO Unit := do
  let buffer <- stream.read buffer_size
  if buffer.isEmpty then
    pure ()
  else
    let stdout <- IO.getStdout
    stdout.write buffer
    dump stream

-- read the content of the file.

def file_stream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let file_exists <- filename.pathExists
  if not file_exists then
    let stderr <- IO.getStderr
    stderr.putStrLn s!"File not found: {filename}"
    pure none
  else
    let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle handle))


def process (exit_code : UInt32) (args : List String) : IO UInt32 := do
  match args with
  | []  => pure exit_code
  | "-" :: args =>
    let stdin <- IO.getStdin
    dump stdin
    process exit_code args
  | filename :: args =>
    let stream <- file_stream (filename)
    match stream with
      | none =>
        process 1 args
      | some stream =>
        dump stream
        process exit_code args

-- Meow !

def main : List String -> IO UInt32
  | [] => process 0 ["-"]
  | args  => process 0 args
