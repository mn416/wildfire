#!/usr/bin/env python

# Given an CNF file, emit a new CNF file where the variables are
# ordered using Satzoo's static variable ordering heuristic.

import subprocess
import sys
import re
import random
import sets
import math

# =============================================================================
# Misc functions
# =============================================================================

def abort(msg):
  print "ERROR:", msg
  sys.exit(-1)

# =============================================================================
# CNF parser
# =============================================================================

# Check args
if len(sys.argv) != 2:
  print "Usage: varorder.py [FILE]"
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
maxVar = 0
for line in f:
  if line[0] == "c":
    print line,
    continue
  if line[0] == "p":
    print line,
    continue
  for word in line.split():
    lit = int(word)
    lits.append(lit)
    if lit != 0 and abs(lit) not in variables:
      variables[abs(lit)] = 1
      numVars = numVars+1
    if abs(lit) > maxVar:
      maxVar = abs(lit)

if maxVar != numVars:
  abort("Variable ids are not contiguous")

# Split into clauses
clauses = []
while True:
  try: n = lits.index(0)
  except: break
  clauses.append(lits[0:n])
  lits = lits[n+1:]
numClauses = len(clauses)

# Number of literals
numLits = len([lit for c in clauses for lit in c])

# =============================================================================
# Apply ordering heuristic
# =============================================================================

def idx(lit):
  return numVars + 1 + lit

# Clear activity
activity = [0] * (numVars+1)

# Do simple vairable activity heuristic
for c in clauses:
  add = math.pow(2, -len(c))
  for lit in c:
    activity[abs(lit)] = activity[abs(lit)] + add

# Calculate the initial "heat" of all clauses
occurs = [] # Map literal to list of clause indices
for i in range(0, 2*(numVars+1)):
  occurs.append([])
heat = [] # Pairs of heat and clause index
for i in range(0, numClauses):
  c = clauses[i]
  total = 0.0
  for lit in c:
    occurs[idx(lit)].append(i)
    total = total + activity[abs(lit)]
  heat.append((total, i))

# Bump heat for clauses whose variables occur in other hot clauses
iter_size = 0.0
for c in clauses:
  for lit in c:
    iter_size = iter_size + len(occurs[idx(lit)])
iterations = int(min((numLits / iter_size)*100, 10))
disapation = 1.0 / iterations
for j in range(0, iterations):
  for i in range(0, numClauses):
    c = clauses[i]
    for lit in c:
      os = occurs[idx(lit)]
      for o in occurs[idx(lit)]:
        newHeat = heat[i][0] + (heat[o][0] * disapation)
        heat[i] = (newHeat, heat[i][1])

# Set activity according to hot clauses
heat.sort()
heat.reverse()
for i in range(0, numVars+1):
  activity[i] = 0

extra = 1e200
for p in heat:
  c = clauses[p[1]]
  for lit in c:
    if activity[abs(lit)] == 0:
      activity[abs(lit)] = extra
      extra = extra * 0.995

pairs = zip(activity[1:], range(1, numVars+1))
pairs.sort()
pairs.reverse()

subst = [0] * (numVars+1)
count = 1
for p in pairs:
  subst[p[1]] = count
  count = count+1

# =============================================================================
# Emit new CNF
# =============================================================================

for c in clauses:
  for lit in c:
    newVar = subst[abs(lit)]
    newLit = -newVar if lit < 0 else newVar
    print newLit,
  print 0
