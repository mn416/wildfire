#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// =========
// Constants
// =========

#define MAX_LITS (1<<20)
#define MAX_VARS (1<<20)
#define UNDO_STACK_SIZE (1<<20)

// =====
// Types
// =====

typedef int VarId;

typedef enum { Unbound, False, True } Val;

typedef struct {
  int level;
  Val val;
} Var;

typedef struct {
  Val* ptr;
} UndoItem;

// ================
// Global variables
// ================

Var vars[MAX_VARS];
int lits[MAX_LITS];
int numLits = 0;
int numClauses = 0;
int numVars = 0;
UndoItem undo[UNDO_STACK_SIZE];
int undoTop = 0;
int decisionLevel = 0;
int conflict[MAX_VARS];
int pos[MAX_VARS];
int neg[MAX_VARS];

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
  while (1) {
    if (fscanf(fp, " p cnf %i %i ", &numVars, &numClauses) == 2)
      break;
    else {
      int c = fgetc(fp);
      if (c == EOF) {
        fprintf(stderr, "Unexpected EOF\n");
        exit(EXIT_FAILURE);
      }
    }
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
}

// ===================
// Variable assignment
// ===================

inline void assign(VarId v, Val x)
{
  UndoItem u;
  u.ptr = &vars[v].val;
  undo[undoTop++] = u;
  vars[v].val = x;
  vars[v].level = decisionLevel;
}

inline void newDecisionLevel()
{
  UndoItem u;
  u.ptr = NULL;
  undo[undoTop++] = u;
  decisionLevel++;
}

inline void undoDecisionLevel()
{
  decisionLevel--;
  while (undoTop != 0) {
    undoTop--;
    UndoItem u = undo[undoTop];
    if (u.ptr == NULL) return;
    *u.ptr = Unbound;
  }
}

// ================
// Unit propagation
// ================

inline int max(int x, int y) { return x > y ? x : y; }
inline int min(int x, int y) { return x < y ? x : y; }

// Returns true if unsatisfiable, false otherwise
bool propagate(int* count)
{
  bool sat, changed;
  int unboundCount, unboundLit;

  *count = 0;
  changed = true;
  while (changed) {
    changed = false;
    unboundCount = 0;
    sat = false;
    int i = 0;
    for (int j = 0; j < numLits; j++) {
      int lit = lits[j];
      int v = abs(lit);
      if (lit == 0) {
        if (!sat) {
          if (unboundCount == 0) {
            int k = j-1;
            while (k >= 0) {
              lit = lits[k];
              if (lit == 0) break;
              conflict[abs(lit)]++;
              k--;
            }
            return true;
          }
          else if (unboundCount == 1) {
            *count = *count + 1;
            assign(abs(unboundLit), unboundLit < 0 ? False : True);
            changed = true;
          }
        }
        unboundCount = 0;
        sat = false;
        i = j+1;
      }
      else {
        if ((lit < 0 && vars[v].val == False) ||
            (lit > 0 && vars[v].val == True)) sat = true;
        else if (vars[v].val == Unbound) {
          unboundCount++;
          unboundLit = lit;
        }
      }
    }
  }

  return false;
}

// ==================
// Variable selection
// ==================

bool pickRandom(int* decisionVar)
{
  int r = rand() % numVars;
  for (int i = 0; i < numVars; i++) {
    int v = ((r+i) % numVars) + 1;
    if (vars[v].val == Unbound) {
      *decisionVar = v;
      return true;
    }
  }

  return false;
}

bool pickPopular(int* decisionVar)
{
  for (int i = 0; i < numVars; i++) pos[i] = neg[i] = 0;
  for (int i = 0; i < numLits; i++) {
    bool sat = false;
    for (int j = i; j < numLits; j++) {
      int lit = lits[j];
      int v = abs(lit);
      if (v == 0) break;
      if ((lit < 0 && vars[v].val == False) ||
          (lit > 0 && vars[v].val == True)) sat = true;
    }

    while (1) {
      int lit = lits[i];
      int v = abs(lit);
      if (v == 0) break;
      if (!sat && vars[v].val == Unbound) {
        if (lit < 0) neg[v]++; else pos[v]++;
      }
      i++;
    }
  }

  int max = 0;
  int found = false;
  for (int i = 1; i < numVars; i++) {
    if (vars[i].val == Unbound) {
      if (pos[i] >= max) {
        max = pos[i];
        *decisionVar = i;
        found = true;
      }
      if (neg[i] >= max) {
        max = neg[i];
        *decisionVar = i;
        found = true;
      }
    }
  }

  return found;
}

// Returns true if satisfiable
bool probe()
{
  for (;;) {
    int count;
    bool unsat = propagate(&count);
    if (unsat) return false;

    int decisionVar;
    bool found = pickRandom(&decisionVar);
    //bool found = pickPopular(&decisionVar);
    if (! found) return true;

    Val val = (rand() & 1) ? True : False;
    assign(decisionVar, val);
  }
}

bool pick(int* decisionVar)
{
  for (int v = 1; v <= numVars; v++)
    conflict[v] = 0;

  for (int i = 0; i < 1000; i++) {
    newDecisionLevel();
    bool sat = probe();
    undoDecisionLevel();
  }

  int maxVal = 0;
  int found = false;
  for (int v = 1; v <= numVars; v++) {
    if (vars[v].val == Unbound && conflict[v] >= maxVal) {
      found = true;
      maxVal = conflict[v];
      *decisionVar = v;
    }
  }

  return found;
}

// =======
// Helpers
// =======

typedef struct {
  int fst;
  int snd;
} Pair;

Pair pairs[MAX_VARS];

int cmpSnd(const void* a, const void* b)
{
  Pair* p = (Pair*) a;
  Pair* q = (Pair*) b;
  return q->snd - p->snd;
}

int subst[MAX_VARS];

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
  for (int v = 0; v < MAX_VARS; v++) vars[v].val = Unbound;

  for (int v = 1; v <= numVars; v++)
    conflict[v] = 0;

  for (int i = 0; i < 10000; i++) {
    newDecisionLevel();
    bool sat = probe();
    undoDecisionLevel();
  }

  int maxVal = 0;
  int found = false;
  for (int v = 1; v <= numVars; v++) {
    pairs[v-1].fst = v;
    pairs[v-1].snd = conflict[v];
  }
  qsort(pairs, numVars, sizeof(Pair), cmpSnd);
 
  for (int i = 0; i < numVars; i++) {
    subst[pairs[i].fst] = i+1;
  }

  for (int i = 0; i < numLits; i++) {
    int lit = lits[i];
    if (lit == 0) continue;
    int w = subst[abs(lit)];
    lits[i] = lit < 0 ? -w : w;
  }

  printf("p cnf %i %i\n", numVars, numClauses);
  for (int i = 0; i < numLits; i++) {
    int lit = lits[i];
    if (lit == 0) printf("0\n");
    else printf("%i ", lit);
  }

  return 0;
}
