-- Golomb ruler solver
-- Uses the "shift algorithm" due to W. Rankin & D. McCracken
-- Includes "midpoint reduction" pruning rule
-- This version avoids long bit-vectors through chunking

-- Search for ruler with NumMarks and MaxLength
const NumMarks  = 14
const MaxLength = 127

-- Num bits needed to represent ruler
const NumBits = MaxLength+1

-- Ruler is divided into chunks of this size
const ChunkLen = 40
const ChunkLenMinusOne = ChunkLen-1

-- Number of chunks needed to represent ruler
const NumChunks = (NumBits+ChunkLenMinusOne) / ChunkLen
const LogChunks = log(NumChunks)

-- For midpoint reduction rule
const MidPoint  = (MaxLength+1)/2
const HalfMarks = (NumMarks+1)/2

-- Compiler options
opt StackWidth = ChunkLen
opt StackDepth = 1024
opt UndoDepth  = 1024

-- Program state
var ruler  : bit(LogChunks) -> bit(ChunkLen)
var dist   : bit(LogChunks) -> bit(ChunkLen)
var marks  : bit(5) = 1
var len    : bit(10)
var r      : bit(ChunkLen)
var d      : bit(ChunkLen)
var insert : bit(1)
var carry  : bit(1)
var i      : bit(LogChunks+1)
var ok     : bit(1)

ruler[0] := 1 ;
while marks /= NumMarks do
  if len == MaxLength then fail end ;
  if (marks < HalfMarks) & (len > MidPoint) then fail end ;

  i := 0 || ok := 1 ;
  while ok & (i /= NumChunks) do
       r := ruler[i]
    || d := dist[i]
     ; ok := ok & ((r&d) == 0)
    || i := i+1
  end ;

  insert := 0 || carry := 0 ;
  if ok then (insert := 1 || carry := 1 || marks := marks+1) ? skip end ;

  i := 0 || len := len+1 ;
  while i /= NumChunks do
       r := ruler[i]
    || d := dist[i]
     ; ruler[i] := (r << 1) | (0 ++ carry)
    || if insert then dist[i] := d|r end
     ; carry := msb(r)
    || i := i+1
  end
end
