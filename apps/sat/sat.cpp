#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// =========
// Constants
// =========

#define MAX_LITS 65536
#define MAX_VARS 65536
#define UNDO_STACK_SIZE 65536

// =====
// Types
// =====

typedef int VarId;

enum Val { Unbound, False, True };

struct UndoItem {
  int* ptr;
  int  old;
};

struct Clause {
  int len;
  int* lits;
  Clause** next;
};

// ================
// Global variables
// ================

Val vars[MAX_VARS+1];
int lits[MAX_LITS];
int numLits = 0;
int numClauses = 0;
int numVars = 0;
UndoItem undo[UNDO_STACK_SIZE];
int undoTop = 0;
Clause* clause[MAX_VARS+1];
int prevChoice = 0;

// ==========
// CNF Parser
// ==========

void parse(const char* filename)
{
  int literal;
  FILE* fp = fopen(filename, "rt");
  if (fp == NULL) {
    fprintf(stderr, "Can't open file '%s'\n", filename);
    exit(EXIT_FAILURE);
  }
  if (fscanf(fp, " p cnf %i %i ", &numVars, &numClauses) != 2) {
    fprintf(stderr, "Parse error: expecting 'p cnf'\n");
    exit(EXIT_FAILURE);
  }
  if (numVars >= MAX_VARS) {
    fprintf(stderr, "Number of variables exceeds MAX_VARS\n");
    exit(EXIT_FAILURE);
  }
  while (fscanf(fp, " %i ", &literal) == 1) {
    if (abs(literal) > numVars) {
      fprintf(stderr, "Variable id exceeds number of variables\n");
      exit(EXIT_FAILURE);
    }
    if (numLits == MAX_LITS) {
      fprintf(stderr, "Number of literals exceeds MAX_LITS\n");
      exit(EXIT_FAILURE);
    }
    lits[numLits++] = literal;
  }
  fclose(fp);

  for (int i = 0; i <= numVars; i++)
    clause[i] = NULL;

  int clauseId = 0;
  int litCount = 0;
  for (int i = 0; i < numLits; i++) {
    int lit = lits[i];
    if (lit == 0) {
      Clause* cl = new Clause;
      cl->len = litCount;
      cl->lits = new int [litCount];
      cl->next = new Clause* [litCount];
      int k = 0;
      for (int j = i-litCount; j < i; j++) {
        int v = abs(lits[j]);
        cl->lits[k] = lits[j];
        cl->next[k] = clause[v];
        clause[v] = cl;
        k++;
      }
      litCount = 0;
    }
    else
      litCount++;
  }
}

// ===================
// Variable assignment
// ===================

inline void assign(int* p, int x)
{
  UndoItem u;
  u.ptr = p;
  u.old = *p;
  undo[undoTop++] = u;
  *p = x;
}

inline void assignVar(VarId v, Val x)
{
  assign((int*) &vars[v], (int) x);
}

inline void newDecisionLevel()
{
  UndoItem u;
  u.ptr = NULL;
  undo[undoTop++] = u;
}

inline void undoDecisionLevel()
{
  while (undoTop != 0) {
    undoTop--;
    UndoItem u = undo[undoTop];
    if (u.ptr == NULL) return;
    *u.ptr = u.old;
  }
}

// ================
// Unit propagation
// ================

// If unsat returns true, otherwise false
bool propagate(VarId from)
{
  Clause* cl = clause[from];
  while (cl != NULL) {
    int unboundCount = 0;
    int unboundLit;
    Clause* next = NULL;
    bool sat = false;
    for (int i = 0; i < cl->len; i++) {
      int lit = cl->lits[i];
      int v = abs(lit);
      if (v == from) next = cl->next[i];
      if (vars[v] == Unbound) { unboundLit = lit; unboundCount++; }
      sat = sat || (lit > 0 && vars[v] == True || lit < 0 && vars[v] == False);
    }
    if (!sat) {
      if (unboundCount == 0) { return true; }
      else if (unboundCount == 1) {
        assignVar(abs(unboundLit), unboundLit < 0 ? False : True);
        bool unsat = propagate(abs(unboundLit));
        if (unsat) { return true; }
      }
    }
    cl = next;
  }
  return false;
}

// ==================
// Variable selection
// ==================

// Follow static variable order
bool pickStatic(VarId* choice)
{
  int i = prevChoice+1;
  for (;;) {
    if (i > numVars) return false;
    if (vars[i] == Unbound) break;
    i++;
  }
  prevChoice = i;
  *choice = i;
  return true;
}

// ======
// Solver
// ======

// Return true if unsatisfiable, false if satisfiable
bool solve()
{
  int decisionVar;
  int savedChoice = prevChoice;
  if (pickStatic(&decisionVar)) {
    newDecisionLevel();
    assignVar(decisionVar, True);
    bool unsat = propagate(decisionVar) || solve();
    if (unsat) {
      undoDecisionLevel();
      newDecisionLevel();
      assignVar(decisionVar, False);
      unsat = propagate(decisionVar) || solve();
      if (unsat) {
        undoDecisionLevel();
        prevChoice = savedChoice;
      }
      return unsat;
    }
    else {
      prevChoice = savedChoice;
      return false;
    }
  }
  else {
    return false;
  }
}

// ====
// Main
// ====

int main(int argc, char* argv[])
{
  if (argc != 2) {
    fprintf(stderr, "Usage: sat [FILE]\n");
    exit(EXIT_FAILURE);
  }

  srand(0);

  parse(argv[1]);

  // Initialisation
  for (int v = 0; v <= MAX_VARS; v++) vars[v] = Unbound;

  bool unsat = solve();
  if (unsat) printf("UNSAT\n");
  else printf("SAT\n");

  return 0;
}
