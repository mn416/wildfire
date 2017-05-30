-- Golomb ruler solver
-- Uses the "shift algorithm" due to W. Rankin & D. McCracken

-- Search for ruler with NumMarks and MaxLength
const NumMarks  = 10
const MaxLength = 55

-- Num bits needed to represent ruler
const N = MaxLength + 1

-- Compiler options
opt StackWidth = 40
opt ProcessorsPerRom = 2

-- Program state
var ruler : bit(N) = 1  -- Positions of marks on ruler
var dist  : bit(N) = 0  -- Distances measured by ruler
var marks : bit(5) = 1  -- Number of marks made so far

while marks /= NumMarks do
  if msb(ruler) == 1 then fail end ;
  if (ruler & dist) == 0 then
      (ruler := ruler << 1)
    ? (marks := marks + 1 ||
       dist  := dist | ruler ;
       ruler := (ruler << 1) | 1)
  else
    ruler := ruler << 1
  end
end
