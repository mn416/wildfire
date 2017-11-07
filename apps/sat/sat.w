-- Basic SAT Solver with unit propagation

const LogMaxVars    = 11
const LogMaxLits    = 14
const LogStackDepth = 9

-- Options
opt ProcessorsPerROM = 8
opt StackWidth = 20

-- Types
type VarId      = bit(LogMaxVars)
type LitId      = bit(LogMaxLits)
type StackIndex = bit(LogStackDepth)
enum Value      = Unbound | Zero | One | End
struct Lit      = { neg         : bit(1)
                  , id          : VarId
                  , next        : LitId
                  , endOfClause : bit(1)
                  , finalLit    : bit(1) }

-- Array of literals in CNF
var lits : LitId -> Lit = "lits.mif"
-- First literal of first clause containing var
var first : VarId -> LitId = "first.mif"
-- Mapping from variables to values
var vars : VarId -> Value = "vars.mif"
-- Temporary unit propagation stack
var stack : StackIndex => VarId

-- Registers
var solved    : bit(1)
var picked    : bit(1)
var lit       : Lit
var val       : Value
var v         : VarId = 1
var w         : VarId
var unit      : Lit
var i         : LitId
var j         : LitId
var sp        : StackIndex
var sat       : bit(1)
var unbound   : bit(2)

-- Solver
while ~solved do
  -- Pick unbound variable
  picked := 0 ;
  while ~picked do
    val := vars[v] ;
    if (val == Unbound) | (val == End)
      then picked := 1 else v := v+1 end
  end ;

  -- Have all variables been assigned?
  if val == End then
    solved := 1 
  else
    -- Non-deterministic choice
    val := Zero ? val := One ;

    -- Assign variable and push variable to stack
    vars[v] := val || stack[0] := v || sp := 1 ;

    while sp /= 0 do
      -- Pop stack
      w := stack[sp-1] ; sp := sp-1 || i := first[w] ;

      -- Loop over each clause containing variable
      while i /= ~0 do
        -- Loop over each literal in clause
        j := i || lit := 0 || unbound := 0 || sat := 0 ;
        while ~lit.endOfClause do
          lit := lits[j] ;
          val := vars[lit.id] ;
          if lit.id == w then i := lit.next end ||
          if (val == One) & ~lit.neg |
             (val == Zero) & lit.neg then sat := 1 end ||
          if (val == Unbound) & (unbound /= 2) then
            unbound := unbound+1 || unit := lit end ||
          j := j+1
        end ;

        -- Fail if clause is unsatisfiable
        if ~sat & (unbound == 0) then fail end ;

        -- Unit propagation
        if ~sat & (unbound == 1) then
          vars[unit.id] := cond(unit.neg, Zero, One) ||
          stack[sp] := unit.id ;
          sp := sp+1
        end
      end
    end
  end
end
