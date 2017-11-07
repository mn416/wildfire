#!/usr/bin/env python

# Given an CNF file, generate MIF files for the dpll.w program.

import subprocess
import sys
import re
import random
import sets

# =============================================================================
# Parameters
# =============================================================================

LogMaxVars      = 11
LogMaxLits      = 14
LogStackDepth   = 9

# =============================================================================
# Misc functions
# =============================================================================

def abort(msg):
  print "ERROR:", msg
  sys.exit(-1)

# =============================================================================
# Emit MIF file
# =============================================================================

def emitMIF(filename, d, w, contents):
  f = open(filename, "w")
  f.write("DEPTH = " + str(d) + ";\n")
  f.write("WIDTH = " + str(w) + ";\n")
  f.write("ADDRESS_RADIX = DEC ;\n")
  f.write("DATA_RADIX = DEC ;\n")
  f.write("CONTENT\n")
  f.write("BEGIN\n")
  n = len(contents)
  for i in range(d):
    if i < n:
      f.write(str(i) + " : " + str(contents[i]) + " ;\n")
    else:
      f.write(str(i) + " : 0 ;\n")
  f.write("END\n")
  f.close()

# =============================================================================
# CNF parser
# =============================================================================

# Check args
if len(sys.argv) != 2:
  print "Usage: genmif.py [FILE]"
  sys.exit()

# Open trace
if sys.argv[1] == "-":
  f = sys.stdin
else:
  f = open(sys.argv[1], 'r')
  if f == None:
    print "File not found: ", sys.argv[1]
    sys.exit()

# Parse trace
lits = []
variables = {}
numVars = 0
for line in f:
  if line[0] == "c": continue
  if line[0] == "p": continue
  for word in line.split():
    v = int(word)
    lits.append(v)
    numVars = max(numVars, abs(v)+1)
    if abs(v) in variables:
      variables[abs(v)] = variables[abs(v)]+1
    else:
      variables[abs(v)] = 1

# =============================================================================
# MIF generator
# =============================================================================

# Check number of variables
if numVars >= 2**LogMaxVars:
  abort("Max variables exceeded")

# Split into clauses
clauses = []
while True:
  try: n = lits.index(0)
  except: break
  clauses.append(lits[0:n])
  lits = lits[n+1:]
numClauses = len(clauses)

# Check number of literals
lits = [lit for clause in clauses for lit in clause]
numLits = len(lits)
if numLits >= (2**LogMaxLits)-1:
  abort("Max literals exceeded")

# Determine index of each clause
clauseIds = []
clauseId = 0
for clause in clauses:
  clauseIds.append(clauseId)
  clauseId = clauseId + len(clause)

# Find index of clause containing next occurence of variable
def findNext(i, v):
  while i < numClauses:
    for lit in clauses[i]:
      if abs(lit) == v:
        return clauseIds[i]
    i = i+1
  return (2**LogMaxLits)-1

# For each literal, determine literal index of clause
# containing next occurence of that variable
nextClause = []
for i in range(0, numClauses):
  nextClause.append([])
  for lit in clauses[i]:
    nextClause[i].append(findNext(i+1, abs(lit)))

# Encode lits.mif
lits = []
for i in range(0, numClauses):
  finalClause = i+1 == numClauses
  n = len(clauses[i])
  for j in range(0, n):
    endOfClause = 1 if j+1 == n else 0
    finalLit = 1 if finalClause and endOfClause else 0
    lit = 1 if clauses[i][j] < 0 else 0
    lit = lit | (abs(clauses[i][j]) << 1)
    lit = lit | (nextClause[i][j] << (1+LogMaxVars))
    lit = lit | (endOfClause << (1+LogMaxVars+LogMaxLits))
    lit = lit | (finalLit << (2+LogMaxVars+LogMaxLits))
    lits.append(lit)

emitMIF("lits.mif", 2**LogMaxLits, 3+LogMaxVars+LogMaxLits, lits)

# Encode first.mif
first = []
for v in range(0, numVars):
  first.append(findNext(0, v))

emitMIF("first.mif", 2**LogMaxVars, LogMaxLits, first)

# Encode vars.mif
vas = [1]
for v in range(1, numVars):
  if v in variables:
    vas.append(0)
  else:
    vas.append(1)
vas.append(3)

emitMIF("vars.mif", 2**LogMaxVars, 2, vas)
