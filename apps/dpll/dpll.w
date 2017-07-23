-- SAT Solver using DPLL algorithm

const LogMaxVars    = 9
const LogMaxLits    = 12
const LogStackDepth = 10
const LogMaxOccs    = 4

-- Inferred constants
const MaxVars = 1 << LogMaxVars

-- Types
type VarId      = bit(LogMaxVars)
type LitId      = bit(LogMaxLits)
type StackIndex = bit(LogStackDepth)
type OccCount   = bit(LogMaxOccs)
enum Value      = Undef | Zero | One
rec  Lit        = { neg         : bit(1)
                  , id          : VarId
                  , next        : LitId
                  , endOfClause : bit(1)
                  , finalLit    : bit(1) }

-- Arrays
var lits  : LitId      -> Lit    =   "lits.mif"
var vars  : VarId      -> Value
var stack : StackIndex => Lit 
var freq  : VarId      => OccCount

-- Registers
var done      : bit(1)
var stop      : bit(1)
var update    : bit(1)
var lit       : Lit
var branchLit : Lit
var val       : Value
var i         : LitId
var j         : LitId
var k         : LitId
var sp        : StackIndex
var root      : Lit
var iter      : bit(1)
var last      : bit(1)
var sat       : bit(1)
var undef     : bit(2)
var unit      : Lit
var nextj     : LitId
var v         : bit(LogMaxVars+1)
var occs      : OccCount
var maxOccs   : OccCount

-- Solver
while ~done do
  -- Initialise frequency table
  v := 0 ;
  while v /= MaxVars do freq[v] := 0 ; v := v+1 end ;

  -- Find the most frequently occurring unassigned variable
  i := 0 || j := 0 || sat := 0 || update := 0 || stop := 0 || maxOccs := 0 ;
  while ~stop do
    lit  := lits[i] ;
    val  := vars[lit.id] ||
    occs := freq[lit.id] ;
    i    := i+1 ||
    occs := occs+1 ||
    stop := lit.finalLit ||
    if (val == One) & ~lit.neg |
       (val == Zero) & lit.neg then sat := 1 end ;
    if update then
      if val == Undef then
         freq[lit.id] := occs ||
         if occs >= maxOccs then maxOccs := occs || branchLit := lit end
      end
    end ;
    if lit.endOfClause then
      if ~update & ~sat then
        update := 1 || i := j || stop := 0
      else
        update := 0 || j := i
      end ;
      sat := 0
    end
  end ;

  -- Make new assignment
  if maxOccs == 0 then
    done := 1 
  else
    val := Zero ? val := One ;

    -- Assign variable and push literal to stack
    vars[branchLit.id] := val ||
    stack[0] := branchLit ||
    sp := 1 ;

    while sp /= 0 do
      root := stack[sp-1] ;
      sp := sp-1 || j := root.next || iter := 1 ;

      -- Loop over each clause containing literal
      while iter do
        -- Loop over each literal in clause
        undef := 0 || k := 0 || sat := 0 || last := 0 ;
        while ~last do
          lit := lits[j+k] ;
          val := vars[lit.id] ;
          if lit.id == root.id then nextj := lit.next end ||
          if (val == One) & ~lit.neg |
             (val == Zero) & lit.neg then sat := 1 end ||
          if (val == Undef) & (undef /= 2) then
            undef := undef+1 || unit := lit end ||
          last := lit.endOfClause ||
          k := k+1
        end ;

        -- Fail if clause is unsatisfiable
        if ~sat & (undef == 0) then fail end ;

        -- Unit propagation
        if ~sat & (undef == 1) then
          vars[unit.id] := cond(unit.neg, Zero, One) ||
          stack[sp] := unit ;
          sp := sp+1
        end ;

        j := nextj || if nextj == root.next then iter := 0 end
      end
    end
  end
end
