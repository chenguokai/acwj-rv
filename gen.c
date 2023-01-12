#include "defs.h"
#include "data.h"
#include "decl.h"

// Generic code generator
// Copyright (c) 2019 Warren Toomey, GPL3

// Generate and return a new label number
static int labelid = 1;
int genlabel(void) {
  return (labelid++);
}

static void update_line(struct ASTnode *n) {
  // Output the line into the assembly if we've
  // changed the line number in the AST node
  if (n->linenum != 0 && Line != n->linenum) {
    Line = n->linenum;
    cglinenum(Line);
  }
}

// Generate the code for an IF statement
// and an optional ELSE clause.
static int genIF(struct ASTnode *n, int looptoplabel, int loopendlabel) {
  int Lfalse, Lend;

  // Generate two labels: one for the
  // false compound statement, and one
  // for the end of the overall IF statement.
  // When there is no ELSE clause, Lfalse _is_
  // the ending label!
  Lfalse = genlabel();
  if (n->right)
    Lend = genlabel();

  // Generate the condition code followed
  // by a jump to the false label.
  genAST(n->left, Lfalse, NOLABEL, NOLABEL, n->op);
  genfreeregs(NOREG);

  // Generate the true compound statement
  genAST(n->mid, NOLABEL, looptoplabel, loopendlabel, n->op);
  genfreeregs(NOREG);

  // If there is an optional ELSE clause,
  // generate the jump to skip to the end
  if (n->right)
    cgjump(Lend);

  // Now the false label
  cglabel(Lfalse);

  // Optional ELSE clause: generate the
  // false compound statement and the
  // end label
  if (n->right) {
    genAST(n->right, NOLABEL, NOLABEL, loopendlabel, n->op);
    genfreeregs(NOREG);
    cglabel(Lend);
  }

  return (NOREG);
}

// Generate the code for a WHILE statement
static int genWHILE(struct ASTnode *n) {
  int Lstart, Lend;

  // Generate the start and end labels
  // and output the start label
  Lstart = genlabel();
  Lend = genlabel();
  cglabel(Lstart);

  // Generate the condition code followed
  // by a jump to the end label.
  genAST(n->left, Lend, Lstart, Lend, n->op);
  genfreeregs(NOREG);

  // Generate the compound statement for the body
  genAST(n->right, NOLABEL, Lstart, Lend, n->op);
  genfreeregs(NOREG);

  // Finally output the jump back to the condition,
  // and the end label
  cgjump(Lstart);
  cglabel(Lend);
  return (NOREG);
}

// Generate the code for a SWITCH statement
static int genSWITCH(struct ASTnode *n) {
  int *caseval, *caselabel;
  int Ljumptop, Lend;
  int i, reg, defaultlabel = 0, casecount = 0;
  struct ASTnode *c;

  // Create arrays for the case values and associated labels.
  // Ensure that we have at least one position in each array.
  caseval = (int *) malloc((n->a_intvalue + 1) * sizeof(int));
  caselabel = (int *) malloc((n->a_intvalue + 1) * sizeof(int));

  // Generate labels for the top of the jump table, and the
  // end of the switch statement. Set a default label for
  // the end of the switch, in case we don't have a default.
  Ljumptop = genlabel();
  Lend = genlabel();
  defaultlabel = Lend;

  // Output the code to calculate the switch condition
  reg = genAST(n->left, NOLABEL, NOLABEL, NOLABEL, 0);
  cgjump(Ljumptop);
  genfreeregs(reg);

  // Walk the right-child linked list to
  // generate the code for each case
  for (i = 0, c = n->right; c != NULL; i++, c = c->right) {

    // Get a label for this case. Store it
    // and the case value in the arrays.
    // Record if it is the default case.
    caselabel[i] = genlabel();
    caseval[i] = c->a_intvalue;
    cglabel(caselabel[i]);
    if (c->op == A_DEFAULT)
      defaultlabel = caselabel[i];
    else
      casecount++;

    // Generate the case code. Pass in the end label for the breaks.
    // If case has no body, we will fall into the following body.
    if (c->left)
      genAST(c->left, NOLABEL, NOLABEL, Lend, 0);
    genfreeregs(NOREG);
  }

  // Ensure the last case jumps past the switch table
  cgjump(Lend);

  // Now output the switch table and the end label.
  cgswitch(reg, casecount, Ljumptop, caselabel, caseval, defaultlabel);
  cglabel(Lend);
  return (NOREG);
}

// Generate the code for an
// A_LOGAND or A_LOGOR operation
static int gen_logandor(struct ASTnode *n) {
  // Generate two labels
  int Lfalse = genlabel();
  int Lend = genlabel();
  int reg;

  // Generate the code for the left expression
  // followed by the jump to the false label
  reg = genAST(n->left, NOLABEL, NOLABEL, NOLABEL, 0);
  cgboolean(reg, n->op, Lfalse);
  genfreeregs(NOREG);

  // Generate the code for the right expression
  // followed by the jump to the false label
  reg = genAST(n->right, NOLABEL, NOLABEL, NOLABEL, 0);
  cgboolean(reg, n->op, Lfalse);
  genfreeregs(reg);

  // We didn't jump so set the right boolean value
  if (n->op == A_LOGAND) {
    cgloadboolean(reg, 1);
    cgjump(Lend);
    cglabel(Lfalse);
    cgloadboolean(reg, 0);
  } else {
    cgloadboolean(reg, 0);
    cgjump(Lend);
    cglabel(Lfalse);
    cgloadboolean(reg, 1);
  }
  cglabel(Lend);
  return (reg);
}

// Generate the code to copy the arguments of a
// function call to its parameters, then call the
// function itself. Return the register that holds 
// the function's return value.
static int gen_funccall(struct ASTnode *n) {
  struct ASTnode *gluetree = n->left;
  int reg;
  int numargs = 0;

  // Save the registers before we copy the arguments
  cgspillregs();

  // If there is a list of arguments, walk this list
  // from the last argument (right-hand child) to the
  // first
  while (gluetree) {
    // Calculate the expression's value
    reg = genAST(gluetree->right, NOLABEL, NOLABEL, NOLABEL, gluetree->op);
    // Copy this into the n'th function parameter: size is 1, 2, 3, ...
    cgcopyarg(reg, gluetree->a_size);
    // Keep the first (highest) number of arguments
    if (numargs == 0)
      numargs = gluetree->a_size;
    gluetree = gluetree->left;
  }

  // Call the function, clean up the stack (based on numargs),
  // and return its result
  return (cgcall(n->sym, numargs));
}

// Generate code for a ternary expression
static int gen_ternary(struct ASTnode *n) {
  int Lfalse, Lend;
  int reg, expreg;

  // Generate two labels: one for the
  // false expression, and one for the
  // end of the overall expression
  Lfalse = genlabel();
  Lend = genlabel();

  // Generate the condition code followed
  // by a jump to the false label.
  genAST(n->left, Lfalse, NOLABEL, NOLABEL, n->op);
  // genfreeregs(NOREG);

  // Get a register to hold the result of the two expressions
  reg = cgallocreg();

  // Generate the true expression and the false label.
  // Move the expression result into the known register.
  expreg = genAST(n->mid, NOLABEL, NOLABEL, NOLABEL, n->op);
  cgmove(expreg, reg);
  cgfreereg(expreg);
  cgjump(Lend);
  cglabel(Lfalse);

  // Generate the false expression and the end label.
  // Move the expression result into the known register.
  expreg = genAST(n->right, NOLABEL, NOLABEL, NOLABEL, n->op);
  cgmove(expreg, reg);
  cgfreereg(expreg);
  cglabel(Lend);
  return (reg);
}

// Given an AST, an optional label, and the AST op
// of the parent, generate assembly code recursively.
// Return the register id with the tree's final value.
int genAST(struct ASTnode *n, int iflabel, int looptoplabel,
	   int loopendlabel, int parentASTop) {
  int leftreg = NOREG, rightreg = NOREG;

  // Empty tree, do nothing
  if (n == NULL)
    return (NOREG);

  // Update the line number in the output
  update_line(n);

  // We have some specific AST node handling at the top
  // so that we don't evaluate the child sub-trees immediately
  switch (n->op) {
  case A_IF:
    return (genIF(n, looptoplabel, loopendlabel));
  case A_WHILE:
    return (genWHILE(n));
  case A_SWITCH:
    return (genSWITCH(n));
  case A_FUNCCALL:
    return (gen_funccall(n));
  case A_TERNARY:
    return (gen_ternary(n));
  case A_LOGOR:
    return (gen_logandor(n));
  case A_LOGAND:
    return (gen_logandor(n));
  case A_GLUE:
    // Do each child statement, and free the
    // registers after each child
    if (n->left != NULL)
      genAST(n->left, iflabel, looptoplabel, loopendlabel, n->op);
    genfreeregs(NOREG);
    if (n->right != NULL)
      genAST(n->right, iflabel, looptoplabel, loopendlabel, n->op);
    genfreeregs(NOREG);
    return (NOREG);
  case A_FUNCTION:
    // Generate the function's preamble before the code
    // in the child sub-tree
    cgfuncpreamble(n->sym);
    genAST(n->left, NOLABEL, NOLABEL, NOLABEL, n->op);
    cgfuncpostamble(n->sym);
    return (NOREG);
  }

  // General AST node handling below

  // Get the left and right sub-tree values
  if (n->left)
    leftreg = genAST(n->left, NOLABEL, NOLABEL, NOLABEL, n->op);
  if (n->right)
    rightreg = genAST(n->right, NOLABEL, NOLABEL, NOLABEL, n->op);

  switch (n->op) {
  case A_ADD:
    return (cgadd(leftreg, rightreg));
  case A_SUBTRACT:
    return (cgsub(leftreg, rightreg));
  case A_MULTIPLY:
    return (cgmul(leftreg, rightreg));
  case A_DIVIDE:
    return (cgdivmod(leftreg, rightreg, A_DIVIDE));
  case A_MOD:
    return (cgdivmod(leftreg, rightreg, A_MOD));
  case A_AND:
    return (cgand(leftreg, rightreg));
  case A_OR:
    return (cgor(leftreg, rightreg));
  case A_XOR:
    return (cgxor(leftreg, rightreg));
  case A_LSHIFT:
    return (cgshl(leftreg, rightreg));
  case A_RSHIFT:
    return (cgshr(leftreg, rightreg));
  case A_EQ:
  case A_NE:
  case A_LT:
  case A_GT:
  case A_LE:
  case A_GE:
    // If the parent AST node is an A_IF, A_WHILE or A_TERNARY,
    // generate a compare followed by a jump. Otherwise, compare
    // registers and set one to 1 or 0 based on the comparison.
    if (parentASTop == A_IF || parentASTop == A_WHILE ||
	parentASTop == A_TERNARY)
      return (cgcompare_and_jump
	      (n->op, leftreg, rightreg, iflabel, n->left->type));
    else
      return (cgcompare_and_set(n->op, leftreg, rightreg, n->left->type));
  case A_INTLIT:
    return (cgloadint(n->a_intvalue, n->type));
  case A_STRLIT:
    return (cgloadglobstr(n->a_intvalue));
  case A_IDENT:
    // Load our value if we are an rvalue
    // or we are being dereferenced
    if (n->rvalue || parentASTop == A_DEREF) {
      return (cgloadvar(n->sym, n->op));
    } else
      return (NOREG);
  case A_ASPLUS:
  case A_ASMINUS:
  case A_ASSTAR:
  case A_ASSLASH:
  case A_ASMOD:
  case A_ASSIGN:

    // For the '+=' and friends operators, generate suitable code
    // and get the register with the result. Then take the left child,
    // make it the right child so that we can fall into the assignment code.
    switch (n->op) {
    case A_ASPLUS:
      leftreg = cgadd(leftreg, rightreg);
      n->right = n->left;
      break;
    case A_ASMINUS:
      leftreg = cgsub(leftreg, rightreg);
      n->right = n->left;
      break;
    case A_ASSTAR:
      leftreg = cgmul(leftreg, rightreg);
      n->right = n->left;
      break;
    case A_ASSLASH:
      leftreg = cgdivmod(leftreg, rightreg, A_DIVIDE);
      n->right = n->left;
      break;
    case A_ASMOD:
      leftreg = cgdivmod(leftreg, rightreg, A_MOD);
      n->right = n->left;
      break;
    }

    // Now into the assignment code
    // Are we assigning to an identifier or through a pointer?
    switch (n->right->op) {
    case A_IDENT:
      if (n->right->sym->class == C_GLOBAL ||
	  n->right->sym->class == C_EXTERN ||
	  n->right->sym->class == C_STATIC)
	return (cgstorglob(leftreg, n->right->sym));
      else
	return (cgstorlocal(leftreg, n->right->sym));
    case A_DEREF:
      return (cgstorderef(leftreg, rightreg, n->right->type));
    default:
      fatald("Can't A_ASSIGN in genAST(), op", n->op);
    }
  case A_WIDEN:
    // Widen the child's type to the parent's type
    return (cgwiden(leftreg, n->left->type, n->type));
  case A_RETURN:
    cgreturn(leftreg, Functionid);
    return (NOREG);
  case A_ADDR:
    // If we have a symbol, get its address. Otherwise,
    // the left register already has the address because
    // it's a member access
    if (n->sym != NULL)
      return (cgaddress(n->sym));
    else
      return (leftreg);
  case A_DEREF:
    // If we are an rvalue, dereference to get the value we point at,
    // otherwise leave it for A_ASSIGN to store through the pointer
    if (n->rvalue)
      return (cgderef(leftreg, n->left->type));
    else
      return (leftreg);
  case A_SCALE:
    // Small optimisation: use shift if the
    // scale value is a known power of two
    switch (n->a_size) {
    case 2:
      return (cgshlconst(leftreg, 1));
    case 4:
      return (cgshlconst(leftreg, 2));
    case 8:
      return (cgshlconst(leftreg, 3));
    default:
      // Load a register with the size and
      // multiply the leftreg by this size
      rightreg = cgloadint(n->a_size, P_INT);
      return (cgmul(leftreg, rightreg));
    }
  case A_POSTINC:
  case A_POSTDEC:
    // Load and decrement the variable's value into a register
    // and post increment/decrement it
    return (cgloadvar(n->sym, n->op));
  case A_PREINC:
  case A_PREDEC:
    // Load and decrement the variable's value into a register
    // and pre increment/decrement it
    return (cgloadvar(n->left->sym, n->op));
  case A_NEGATE:
    return (cgnegate(leftreg));
  case A_INVERT:
    return (cginvert(leftreg));
  case A_LOGNOT:
    return (cglognot(leftreg));
  case A_TOBOOL:
    // If the parent AST node is an A_IF or A_WHILE, generate
    // a compare followed by a jump. Otherwise, set the register
    // to 0 or 1 based on it's zeroeness or non-zeroeness
    return (cgboolean(leftreg, parentASTop, iflabel));
  case A_BREAK:
    cgjump(loopendlabel);
    return (NOREG);
  case A_CONTINUE:
    cgjump(looptoplabel);
    return (NOREG);
  case A_CAST:
    return (leftreg);		// Not much to do
  default:
    fatald("Unknown AST operator", n->op);
  }
  return (NOREG);		// Keep -Wall happy
}

void genpreamble(char *filename) {
  cgpreamble(filename);
}
void genpostamble() {
  cgpostamble();
}
void genfreeregs(int keepreg) {
  cgfreeallregs(keepreg);
}
void genglobsym(struct symtable *node) {
  cgglobsym(node);
}

// Generate a global string.
// If append is true, append to
// previous genglobstr() call.
int genglobstr(char *strvalue, int append) {
  int l = genlabel();
  cgglobstr(l, strvalue, append);
  return (l);
}
void genglobstrend(void) {
  cgglobstrend();
}
int genprimsize(int type) {
  return (cgprimsize(type));
}
int genalign(int type, int offset, int direction) {
  return (cgalign(type, offset, direction));
}
