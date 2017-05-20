-- Golomb ruler solver
-- Uses the "shift algorithm" due to Rankin & McCracken

-- Search for ruler with NumMarks and MaxLength
const NumMarks  = 6
const MaxLength = 17

-- Num bits needed to represent ruler
const N = MaxLength + 1

-- Program state
var ruler : bit<N> = 1  -- Positions of marks on ruler
var dist  : bit<N> = 0  -- Distances measured by ruler
var marks : bit<5> = 1  -- Number of marks made so far

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
