-- Golomb ruler solver
-- Uses the "shift algorithm" due to W. Rankin & D. McCracken
-- Includes "midpoint reduction" pruning rule

-- Search for ruler with NumMarks and MaxLength
const NumMarks  = 14
const MaxLength = 126

-- Num bits needed to represent ruler
const N = MaxLength+1

-- For midpoint reduction rule
const MidPoint = (MaxLength+1)/2
const HalfMarks = (NumMarks+1)/2

-- Compiler options
opt StackWidth = 40
opt StackDepth = 1024

-- Program state
var ruler : bit(N)  = 1  -- Positions of marks on ruler
var dist  : bit(N)  = 0  -- Distances measured by ruler
var marks : bit(5)  = 1  -- Number of marks placed
var len   : bit(10) -- Current length

while marks /= NumMarks do
  if (msb(ruler) == 1) |
       ((marks < HalfMarks) & (len > MidPoint)) then fail end ;
  -- Shift algorithm
  if (ruler & dist) == 0 then
      (marks := marks + 1 ||
       dist  := dist | ruler ;
       ruler := (ruler << 1) | 1)
    ? (ruler := ruler << 1)
  else
    ruler := ruler << 1
  end ;
  len := len + 1
end
