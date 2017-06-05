#!/usr/bin/env python

# Given an axe trace, generate MIF files for the sc.w program.

import subprocess
import sys
import re
import random
import sets

# =============================================================================
# Parameters
# =============================================================================

LogMaxAddrs   = 4
LogMaxVals    = 13
LogMaxOps     = 14
LogMaxReaders = 5
LogMaxThreads = 2

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
  for i in range(d):
    if i in contents:
      f.write(str(i) + " : " + str(contents[i]) + " ;\n")
    else:
      f.write(str(i) + " : 0 ;\n")
  f.write("END\n")
  f.close()

# =============================================================================
# Trace parser
# =============================================================================

# Op represents a single operation in a trace

class Op:
  # Concrete syntax of lines of an axe trace
  var   = "(?:M\[|v)(\d*)\]?"
  load  = re.compile("(\d*):\s*"+var+"\s*==\s*(\d*)")
  store = re.compile("(\d*):\s*"+var+"\s*:=\s*(\d*)")

  def __init__(self, line):
    self.op = None
    self.addr = None
    self.readVal = None
    self.writeVal = None

    # Parse a load operation
    match = re.search(self.load, line)
    if match is not None:
      self.op = "LD"
      self.tid, self.addr, self.readVal = match.groups()
      self.tid = int(self.tid)
      self.addr = int(self.addr)
      self.readVal = int(self.readVal)
      return

    # Parse a store operation
    match = re.search(self.store, line)
    if match is not None:
      self.op = "ST"
      self.tid, self.addr, self.writeVal = match.groups()
      self.tid = int(self.tid)
      self.addr = int(self.addr)
      self.writeVal = int(self.writeVal)
      return

    abort("PARSE ERROR")

  def show(self):
    if self.op == "LD":
      result = (str(self.tid) + ": v" + str(self.addr) +
                  " == " + str(self.readVal))
    if self.op == "ST":
      result = (str(self.tid) + ": v" + str(self.addr) +
                  " := " + str(self.writeVal))
    return result

  def isLoad(self):
    return self.op == "LD"

  def value(self):
    if self.op == "LD":
      return self.readVal
    else:
      return self.writeVal

  def setValue(self, x):
    if self.op == "LD":
      self.readVal = x
    else:
      self.writeVal = x

# =============================================================================
# MIF generator
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

# Globals
trace = []
thread = {}
numReaders = {}
zeroReaders = {}

# Parse trace
for line in f:
  i = line.find("#")
  if i >= 0: line = line[0:i]
  if not line.strip(): continue
  op = Op(line)
  trace.append(op)

# Make values unique
unique = 1
subst = {}
for op in trace:
  if op.value() != 0:
    p = (op.addr, op.value())
    if p in subst:
      op.setValue(subst[p])
    else:
      op.setValue(unique)
      subst[p] = unique
      unique = unique+1

#for op in trace:
#  print op.show()

for op in trace:
  val = op.value()
  # Sanity checks
  if op.tid >= 2**LogMaxThreads:
    abort("Max thread exceeded: " + str(op.tid()))
  if val >= 2**LogMaxVals:
    abort("Max value exceeded: " + str(val))
  if op.addr >= 2**LogMaxAddrs:
    abort("Max address exceeded: " + str(op.addr))
  # Count number of readers of each value
  if op.isLoad():
    if val in numReaders:
      numReaders[val] = numReaders[val] + 1
      if numReaders[val] >= 2**LogMaxReaders:
        abort("Max readers exceeded for value " + str(val))
    else:
      numReaders[val] = 1
  # Count number of readers of zero at each address
  if op.isLoad() and val == 0:
    if op.addr in zeroReaders:
      zeroReaders[op.addr] = zeroReaders[op.addr] + 1
      if zeroReaders[op.addr] >= 2**LogMaxReaders:
        abort("Max readers exceeded for address " + str(op.addr))
    else:
      zeroReaders[op.addr] = 1

# Sanity checks
if trace == []:
  abort("Empty trace")

# Split operations by thread id
for op in trace:
  if op.tid not in thread: thread[op.tid] = []
  thread[op.tid].append(op)

# Encode operations
code = {}
ptr = {}
i = 0
for t in range(0, 2**LogMaxThreads):
  ptr[t] = i
  if t in thread:
    for op in thread[t]:
      opcode = 1
      if op.op == "ST": opcode = 2
      opcode = opcode | (op.addr << 2)
      opcode = opcode | (op.value() << (2+LogMaxAddrs))
      code[i] = opcode
      i=i+1
  code[i] = 0
  i=i+1

if i >= 2**LogMaxOps: abort("Max trace size exceeded")

emitMIF("readers.mif", 2**LogMaxVals, LogMaxReaders, numReaders)
emitMIF("mreaders.mif", 2**LogMaxAddrs, LogMaxReaders, zeroReaders)
emitMIF("trace.mif", 2**LogMaxOps, 2+LogMaxAddrs+LogMaxVals, code)
emitMIF("next.mif", 2**LogMaxThreads, LogMaxOps, ptr)
