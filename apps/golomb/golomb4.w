-- Golomb ruler solver
-- Uses the "shift algorithm" due to W. Rankin & D. McCracken
-- Includes "midpoint reduction" pruning rule
-- This version avoids long bit-vectors through chunking

-- Search for ruler with NumMarks and MaxLength
const NumMarks  = 8
const MaxLength = 34

-- Num bits needed to represent ruler
const NumBits = MaxLength+1

-- Ruler is divided into chunks of this size
const ChunkLen = 20
const ChunkLenMinusOne = ChunkLen-1

-- Number of chunks needed to represent ruler
const NumChunks = (NumBits+ChunkLenMinusOne) / ChunkLen
const LogChunks = log(NumChunks)

-- For midpoint reduction rule
const MidPoint = (MaxLength+1)/2
const MidMark  = (NumMarks-1)/2

-- Compiler options
opt StackWidth = ChunkLen

-- Program state
var ruler  : bit(LogChunks) -> bit(ChunkLen)
var dist   : bit(LogChunks) -> bit(ChunkLen)
var marks  : bit(5) = 1
var len    : bit(10)
var r      : bit(ChunkLen)
var d      : bit(ChunkLen)
var insert : bit(1)
var carry  : bit(ChunkLen)
var i      : bit(LogChunks+1)
var ok     : bit(1)

ruler[0] := 1 ;
while marks /= NumMarks do
  if len == MaxLength then fail end ;
  if (marks == MidMark) & (len > MidPoint) then fail end ;

  i := 0 || ok := 1 ;
  while ok & (i /= NumChunks) do
       r := ruler[i]
    || d := dist[i]
     ; ok := ok & ((r&d) == 0)
    || i := i+1
  end ;

  insert := 0 || carry := 0 ;
  if ok then skip ? (insert := 1 || carry := 1 || marks := marks+1) end ;

  i := 0 || len := len+1 ;
  while i /= NumChunks do
       r := ruler[i]
    || d := dist[i]
     ; ruler[i] := (r << 1) | carry
    || if insert then dist[i] := d|r end
     ; carry := r >> ChunkLenMinusOne
    || i := i+1
  end
end
