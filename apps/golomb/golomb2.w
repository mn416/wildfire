-- Golomb ruler solver
-- Uses the "shift algorithm" due to W. Rankin & D. McCracken
-- Includes "midpoint reduction" pruning rule

-- Search for ruler with NumMarks and MaxLength
const NumMarks  = 14
const MaxLength = 127

-- Num bits needed to represent ruler
const N = MaxLength+1

-- Initial value for remaining marks
const RemMarks = NumMarks-1

-- For midpoint reduction rule
const HalfLen = MaxLength/2
const MidMark = (NumMarks+1)/2

-- Compiler options
opt StackWidth = 40

-- Program state
var ruler    : bit(N)  = 1  -- Positions of marks on ruler
var dist     : bit(N)  = 0  -- Distances measured by ruler
var remMarks : bit(5)  = RemMarks  -- Num remaining marks
var remLen   : bit(10) = MaxLength -- Remaining length
var n        : bit(10)

while remMarks /= 0 do
  if msb(ruler) == 1 then fail end ;
  -- Midpoint reduction
  if (remMarks == MidMark) & (remLen < HalfLen) then fail end ;
  -- Shift algorithm
  if (ruler & dist) == 0 then
      (ruler := ruler << 1)
    ? (remMarks := remMarks - 1 ||
       dist  := dist | ruler ;
       ruler := (ruler << 1) | 1)
  else
    ruler := ruler << 1
  end ;
  remLen := remLen - 1
end
