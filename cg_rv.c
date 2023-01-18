#include "defs.h"
#include "data.h"
#include "decl.h"

// Code generator for RISC-V 64
// Copyright (c) 2019 Warren Toomey, GPL3
// Copyright (c) 2023 Guokai Chen, GPL3

// Flag to say which section were are outputting in to
enum { no_seg, text_seg, data_seg } currSeg = no_seg;

// Switch to the text segment
void cgtextseg() {
  if (currSeg != text_seg) {
    fputs("\t.text\n", Outfile);
    currSeg = text_seg;
  }
}

// Switch to the data segment
void cgdataseg() {
  if (currSeg != data_seg) {
    fputs("\t.data\n", Outfile);
    currSeg = data_seg;
  }
}

// Given a scalar type value, return the
// size of the type in bytes.
int cgprimsize(int type) {
  if (ptrtype(type))
    return (8);
  switch (type) {
  case P_CHAR:
    return (1);
  case P_INT:
    return (4);
  case P_LONG:
    return (8);
  default:
    fatald("Bad type in cgprimsize:", type);
  }
  return (0);			// Keep -Wall happy
}

// Given a scalar type, an existing memory offset
// (which hasn't been allocated to anything yet)
// and a direction (1 is up, -1 is down), calculate
// and return a suitably aligned memory offset
// for this scalar type. This could be the original
// offset, or it could be above/below the original
int cgalign(int type, int offset, int direction) {
  int alignment;

  // We don't need to do this on x86-64, but let's
  // align chars on any offset and align ints/pointers
  // on a 4-byte alignment
  switch (type) {
  case P_CHAR:
    break;
  default:
    // Align whatever we have now on a 8-byte alignment.
    // I put the generic code here so it can be reused elsewhere.
    alignment = 4;
    offset = (offset + direction * (alignment - 1)) & ~(alignment - 1);
  }
  return (offset);
}

// Position of next local variable relative to stack base pointer.
// We store the offset as positive to make aligning the stack pointer easier
static int localOffset;

// Position of stack pointer offset relative to stack base pointer.
// We need this to ensure it is aligned on a 16-byte boundary.
static int stackOffset;

// Create the position of a new local variable.
static int newlocaloffset(int size) {
  // Decrement the offset by a minimum of 4 bytes
  // and allocate on the stack
  localOffset += (size > 4) ? size : 4;
  return (-localOffset);
}

// List of available registers and their names.
// We need a list of byte and doubleword registers, too.
// The list also includes the registers used to
// hold function parameters
#define NUMFREEREGS 7
#define FIRSTPARAMREG 14		// Position of first parameter register
#define REG_PARM_NUM 6 // TODO: should be 7
static int freereg[NUMFREEREGS];
static char *reglist[] =
  { "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a7", "a6", "a5", "a4", "a3", "a2", "a1",
  "a0"
};

// We also need the 8-bit and 32-bit register names
static char *breglist[] =
  { "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a7", "a6", "a5", "a4", "a3", "a2", "a1",
  "a0"
};

static char *dreglist[] =
  { "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a7", "a6", "a5", "a4", "a3", "a2", "a1",
  "a0"
};

// Push and pop a register on/off the stack
static void pushreg(int r) {
  fprintf(Outfile, "\taddi sp, sp, -8\n");
  fprintf(Outfile, "\tsd\t%s, 0(sp)\n", reglist[r]);
}

static void popreg(int r) {
  fprintf(Outfile, "\tld\t%s, 0(sp)\n", reglist[r]);
  fprintf(Outfile, "\taddi sp, sp, 8\n");
}


// Set all registers as available.
// But if reg is positive, don't free that one.
void cgfreeallregs(int keepreg) {
  int i;
  // fprintf(Outfile, "# freeing all registers\n");
  for (i = 0; i < NUMFREEREGS; i++)
    if (i != keepreg)
      freereg[i] = 1;
}

// When we need to spill a register, we choose
// the following register and then cycle through
// the remaining registers. The spillreg increments
// continually, so we need to take a modulo NUMFREEREGS
// on it.
static int spillreg = 0;

// Allocate a free register. Return the number of
// the register. Die if no available registers.
int cgallocreg(void) {
  int reg;

  for (reg = 0; reg < NUMFREEREGS; reg++) {
    if (freereg[reg]) {
      freereg[reg] = 0;
      // fprintf(Outfile, "# allocated register %s\n", reglist[reg]);
      return (reg);
    }
  }

  // We have no registers, so we must spill one
  reg = (spillreg % NUMFREEREGS);
  spillreg++;
  // fprintf(Outfile, "# spilling reg %s\n", reglist[reg]);
  pushreg(reg);
  return (reg);
}

// Return a register to the list of available registers.
// Check to see if it's not already there.
void cgfreereg(int reg) {
  if (freereg[reg] != 0) {
    // fprintf(Outfile, "# error trying to free register %s\n", reglist[reg]);
    fatald("Error trying to free register", reg);
  }
  // If this was a spilled register, get it back
  if (spillreg > 0) {
    spillreg--;
    reg = (spillreg % NUMFREEREGS);
    // fprintf(Outfile, "# unspilling reg %s\n", reglist[reg]);
    popreg(reg);
  } else {
    // fprintf(Outfile, "# freeing reg %s\n", reglist[reg]);
    freereg[reg] = 1;
  }
}

static int spilled[NUMFREEREGS];
static int spilled_cnt;

// Spill all registers on the stack
void cgspillregs(void) {
  int i;
  int pushed = 0;
  if (spilled_cnt != 0) {
    printf("Spill may be recursive\n");
    exit(1);
  }
  spilled_cnt = 0;
  for (i = 0; i < NUMFREEREGS; i++) {
    if (freereg[i] == 0) {
      spilled[i] = 1;
      ++spilled_cnt;
    } else {
      spilled[i] = 0;
    }
  }
  if (spilled_cnt)
    fprintf(Outfile, "\taddi sp, sp, %d\n", -spilled_cnt * 8);
  for (i = 0; i < NUMFREEREGS; i++) {
    if (spilled[i]) {
      fprintf(Outfile, "\tsd %s, %d(sp)\n", reglist[i], pushed * 8);
      ++pushed;
    }
    
  }
    //pushreg(i);
}

// Unspill all registers from the stack
static void cgunspillregs(void) {
  int i;
  int poped = 0;
  
  for (i = 0; i < NUMFREEREGS; i++) {
    if (spilled[i]) {
      fprintf(Outfile, "\tld %s, %d(sp)\n", reglist[i], poped * 8);
      ++poped;
    }
  }
  if (poped)
    fprintf(Outfile, "\taddi sp, sp, %d\n", spilled_cnt * 8);
    //popreg(i);
  if (poped != spilled_cnt) {
    printf("Spilled array may be corrupted\n");
    exit(1);
  }
  spilled_cnt = 0;
}

// Print out the assembly preamble
// for one output file
// TODO: rewrite switch table
void cgpreamble(char *filename) {
  cgfreeallregs(NOREG);
  cgtextseg();
  fprintf(Outfile, "\t.file 1 ");
  fputc('"', Outfile);
  fprintf(Outfile, "%s", filename);
  fputc('"', Outfile);
  fputc('\n', Outfile);
}
// Move switch handling to the back
void cgpostamble() {
  fprintf(Outfile,
	  "# internal switch(expr) routine\n"
	  "# a1 = switch table, a0 = expr\n"
	  "\n"
	  "__switch:\n"
	  "        addi sp, sp, -32\n"
	  "        sd a3, 0(sp)\n"
    "        sd a4, 8(sp)\n" /* current iteration */
    "        sd a5, 16(sp)\n" /* taken target address */
    "        sd a6, 24(sp)\n" /* current compare value */
	  "        ld a3, 0(a1)\n" /* iteration count */
    "        li a4, 0\n"
    "        addi a1, a1, 8\n"
	  "__next:\n"
	  "        ld a6, 0(a1)\n"
    "        ld a5, 8(a1)\n"
	  "        bne a6, a0, __no\n"
	  "        add a0, a5, zero\n"
    "        ld a6, 24(sp)\n"
    "        ld a5, 16(sp)\n"
    "        ld a4, 8(sp)\n"
    "        ld a3, 0(sp)\n"
	  "        addi sp, sp, 32\n"
	  "        jr     a0\n"
	  "__no:\n"
	  "        addi a1, a1, 16\n"
    "        addi a4, a4, 1\n"
    "        blt a4, a3, __next\n"
    "        ld a0, 0(a1)\n"
    "        ld a6, 24(sp)\n"
    "        ld a5, 16(sp)\n"
    "        ld a4, 8(sp)\n"
    "        ld a3, 0(sp)\n"
    "        addi sp, sp, 32\n"
    "        jr     a0\n");
}

// Print out a function preamble
void cgfuncpreamble(struct symtable *sym) {
  char *name = sym->name;
  struct symtable *parm, *locvar;
  int cnt;
  int paramOffset = 16;		// Any pushed params start at this stack offset
  int paramReg = FIRSTPARAMREG;	// Index to the first param register in above reg lists

  // Output in the text segment, reset local offset
  cgtextseg();
  localOffset = 0;

  // Output the function start, save the %rsp and %rsp
  if (sym->class == C_GLOBAL)
    fprintf(Outfile, "\t.text\n" "\t.globl\t%s\n" "\t.type\t%s, @function\n", name, name);
  fprintf(Outfile, "\t.balign 16, 0\n" "%s:\n" "\taddi sp, sp, -16\n" "\tsd fp, 8(sp)\n" "\tsd ra, 0(sp)\n" "\tadd fp, sp, zero\n", name);

  // Copy any in-register parameters to the stack, up to six of them
  // The remaining parameters are already on the stack
  for (parm = sym->member, cnt = 1; parm != NULL; parm = parm->next, cnt++) {
    if (cnt > 6) {
      parm->st_posn = paramOffset;
      paramOffset += 8;
    } else {
      parm->st_posn = newlocaloffset(parm->size);
      cgstorlocal(paramReg--, parm);
    }
  }

  // For the remainder, if they are a parameter then they are
  // already on the stack. If only a local, make a stack position.
  for (locvar = Loclhead; locvar != NULL; locvar = locvar->next) {
    locvar->st_posn = newlocaloffset(locvar->size);
  }

  // Align the stack pointer to be a multiple of 16
  // less than its previous value
  stackOffset = (localOffset + 15) & ~15;
  fprintf(Outfile, "\tadd\tsp, sp, %d\n", -stackOffset);
}

// Print out a function postamble
void cgfuncpostamble(struct symtable *sym) {
  cglabel(sym->st_endlabel);
  fprintf(Outfile, "\tadd\tsp, sp, %d\n", stackOffset);
  fputs("\tadd sp, fp, zero\n" "\tld fp, 8(sp)\n" "\tld ra, 0(sp)\n" "\taddi sp, sp, 16\n" "\tret\n", Outfile);
  cgfreeallregs(NOREG);
}

// Load an integer literal value into a register.
// Return the number of the register.
// For x86-64, we don't need to worry about the type.
int cgloadint(int value, int type) {
  // Get a new register
  int r = cgallocreg();

  fprintf(Outfile, "\tli\t%s, %d\n", reglist[r], value);
  return (r);
}

// Load a value from a variable into a register.
// Return the number of the register. If the
// operation is pre- or post-increment/decrement,
// also perform this action.
int cgloadvar(struct symtable *sym, int op) {
  int r, tmpreg, postreg, resreg, offset = 1;

  // Get a new register
  r = cgallocreg();
  tmpreg = cgallocreg();

  // If the symbol is a pointer, use the size
  // of the type that it points to as any
  // increment or decrement. If not, it's one.
  if (ptrtype(sym->type))
    offset = typesize(value_at(sym->type), sym->ctype);

  // Negate the offset for decrements
  if (op == A_PREDEC || op == A_POSTDEC)
    offset = -offset;

  // Load the symbol's address
  if (sym->class == C_LOCAL || sym->class == C_PARAM)
    fprintf(Outfile, "\taddi\t%s, fp, %d\n", reglist[tmpreg], sym->st_posn);
  else
    fprintf(Outfile, "\tla\t%s, %s\n", reglist[tmpreg], sym->name);
  // Load the value from symbol address
  switch (sym->size) {
    case 1:
      // TODO: check if lbu is right
      fprintf(Outfile, "\tlbu\t%s, (%s)\n", reglist[r], reglist[tmpreg]);
      break;
    case 4:
      fprintf(Outfile, "\tlw\t%s, (%s)\n", reglist[r], reglist[tmpreg]);
      break;
    case 8:
      fprintf(Outfile, "\tld\t%s, (%s)\n", reglist[r], reglist[tmpreg]);
  }
  // If we have a pre-operation
  if (op == A_PREINC || op == A_PREDEC) {
    // change the value
    fprintf(Outfile, "\taddi\t%s, %s, %d\n", reglist[r], reglist[r], offset);
    switch (sym->size) {
    case 1:
      fprintf(Outfile, "\tsb\t%s, (%s)\n", reglist[r], reglist[tmpreg]);
      break;
    case 4:
      fprintf(Outfile, "\tsw\t%s, (%s)\n", reglist[r], reglist[tmpreg]);
      break;
    case 8:
      fprintf(Outfile, "\tsd\t%s, (%s)\n", reglist[r], reglist[tmpreg]);
    }
  }
  cgfreereg(tmpreg);

  // If we have a post-operation, get a new register
  if (op == A_POSTINC || op == A_POSTDEC) {
    postreg = cgallocreg();
    resreg = cgallocreg();

    // Load the symbol's address
    if (sym->class == C_LOCAL || sym->class == C_PARAM)
      fprintf(Outfile, "\taddi\t%s, fp, %d\n", reglist[postreg], sym->st_posn);
    else
      fprintf(Outfile, "\tla\t%s, %s\n", reglist[postreg], sym->name);

    // and change the value at that address
    switch (sym->size) {
    case 1:
      // TODO: check if lbu is right
      fprintf(Outfile, "\tlbu\t%s, (%s)\n", reglist[resreg], reglist[postreg]);
      break;
    case 4:
      fprintf(Outfile, "\tlw\t%s, (%s)\n", reglist[resreg], reglist[postreg]);
      break;
    case 8:
      fprintf(Outfile, "\tld\t%s, (%s)\n", reglist[resreg], reglist[postreg]);
    }
    fprintf(Outfile, "\taddi\t%s, %s, %d\n", reglist[resreg], reglist[resreg], offset);
    switch (sym->size) {
    case 1:
      fprintf(Outfile, "\tsb\t%s, (%s)\n", reglist[resreg], reglist[postreg]);
      break;
    case 4:
      fprintf(Outfile, "\tsw\t%s, (%s)\n", reglist[resreg], reglist[postreg]);
      break;
    case 8:
      fprintf(Outfile, "\tsd\t%s, (%s)\n", reglist[resreg], reglist[postreg]);
    }
    // Finally, free the register
    cgfreereg(postreg);
    cgfreereg(resreg);
  }

  // Return the register with the value
  return (r);
}

// Given the label number of a global string,
// load its address into a new register
int cgloadglobstr(int label) {
  // Get a new register
  int r = cgallocreg();
  fprintf(Outfile, "\tla\t%s, L%d\n", reglist[r], label);
  return (r);
}

// Add two registers together and return
// the number of the register with the result
int cgadd(int r1, int r2) {
  fprintf(Outfile, "\tadd\t%s, %s, %s\n", reglist[r1], reglist[r1], reglist[r2]);
  cgfreereg(r2);
  return (r1);
}

// Subtract the second register from the first and
// return the number of the register with the result
int cgsub(int r1, int r2) {
  fprintf(Outfile, "\tsub\t%s, %s, %s\n", reglist[r1], reglist[r1], reglist[r2]);
  cgfreereg(r2);
  return (r1);
}

// Multiply two registers together and return
// the number of the register with the result
int cgmul(int r1, int r2) {
  fprintf(Outfile, "\tmul\t%s, %s, %s\n", reglist[r1], reglist[r1], reglist[r2]);
  cgfreereg(r2);
  return (r1);
}

// Divide or modulo the first register by the second and
// return the number of the register with the result
int cgdivmod(int r1, int r2, int op) {
  if (op == A_DIVIDE)
    fprintf(Outfile, "\tdiv\t%s, %s, %s\n", reglist[r1], reglist[r1], reglist[r2]);
  else
    fprintf(Outfile, "\trem\t%s, %s, %s\n", reglist[r1], reglist[r1], reglist[r2]);
  cgfreereg(r2);
  return (r1);
}

// Bitwise AND two registers
int cgand(int r1, int r2) {
  fprintf(Outfile, "\tand\t%s, %s, %s\n", reglist[r1], reglist[r1], reglist[r2]);
  cgfreereg(r2);
  return (r1);
}

// Bitwise OR two registers
int cgor(int r1, int r2) {
  fprintf(Outfile, "\tor\t%s, %s, %s\n", reglist[r1], reglist[r1], reglist[r2]);
  cgfreereg(r2);
  return (r1);
}

// Bitwise XOR two registers
int cgxor(int r1, int r2) {
  fprintf(Outfile, "\txor\t%s, %s, %s\n", reglist[r1], reglist[r1], reglist[r2]);
  cgfreereg(r2);
  return (r1);
}

// Shift left r1 by r2 bits
int cgshl(int r1, int r2) {
  fprintf(Outfile, "\tsll\t%s, %s, %s\n", reglist[r1], reglist[r1], reglist[r2]);
  cgfreereg(r2);
  return (r1);
}

// Shift right r1 by r2 bits
int cgshr(int r1, int r2) {
  fprintf(Outfile, "\tsrl\t%s, %s, %s\n", reglist[r1], reglist[r1], reglist[r2]);
  cgfreereg(r2);
  return (r1);
}

// Negate a register's value
int cgnegate(int r) {
  fprintf(Outfile, "\tneg\t%s, %s\n", reglist[r], reglist[r]);
  return (r);
}

// Invert a register's value
int cginvert(int r) {
  fprintf(Outfile, "\tnot\t%s, %s\n", reglist[r], reglist[r]);
  return (r);
}

// Logically negate a register's value
int cglognot(int r) {
  fprintf(Outfile, "\tseqz\t%s, %s\n", reglist[r], reglist[r]);
  return (r);
}

// Load a boolean value (only 0 or 1)
// into the given register
void cgloadboolean(int r, int val) {
  fprintf(Outfile, "\tli\t%s, %d\n", reglist[r], val);
}

// Convert an integer value to a boolean value. Jump if
// it's an IF, WHILE, LOGAND or LOGOR operation
int cgboolean(int r, int op, int label) {
  switch (op) {
  case A_IF:
  case A_WHILE:
  case A_LOGAND:
    fprintf(Outfile, "\tbeq\t%s, zero, L%d\n", reglist[r], label);
    break;
  case A_LOGOR:
    fprintf(Outfile, "\tbne\t%s, zero, L%d\n", reglist[r], label);
    break;
  default:
    fprintf(Outfile, "\tsnez\t%s, %s\n", reglist[r], reglist[r]);
  }
  return (r);
}

// Call a function with the given symbol id.
// Pop off any arguments pushed on the stack.
// Return the register with the result
int cgcall(struct symtable *sym, int numargs) {
  int outr;

  // Call the function
  fprintf(Outfile, "\tcall\t%s@plt\n", sym->name);

  // Remove any arguments pushed on the stack
  if (numargs > REG_PARM_NUM)
    fprintf(Outfile, "\taddi\tsp, sp, %d\n", 8 * (numargs - REG_PARM_NUM));

  // Unspill all the registers
  cgunspillregs();

  // Get a new register and copy the return value into it
  outr = cgallocreg();
  fprintf(Outfile, "\tadd\t%s, a0, zero\n", reglist[outr]);
  return (outr);
}

// Given a register with an argument value,
// copy this argument into the argposn'th
// parameter in preparation for a future function call.
// Note that argposn is 1, 2, 3, 4, ..., never zero.
void cgcopyarg(int r, int argposn) {

  // If this is above the sixth argument, simply push the
  // register on the stack. We rely on being called with
  // successive arguments in the correct order for x86-64
  if (argposn > REG_PARM_NUM) {
    fprintf(Outfile, "\taddi\tsp, sp, -8\n" "\tsd\t%s, 0(sp)\n", reglist[r]);
  } else {
    // Otherwise, copy the value into one of the six registers
    // used to hold parameter values
    fprintf(Outfile, "\tadd\t%s, %s, zero\n",
	    reglist[FIRSTPARAMREG - argposn + 1], reglist[r]);
  }
  cgfreereg(r);
}

// Shift a register left by a constant
int cgshlconst(int r, int val) {
  fprintf(Outfile, "\tslli\t%s, %s, %d\n", reglist[r], reglist[r], val);
  return (r);
}

// Store a register's value into a variable
int cgstorglob(int r, struct symtable *sym) {
  int addrreg = cgallocreg();
  fprintf(Outfile, "\tla\t%s, %s\n", reglist[addrreg], sym->name);
  if (cgprimsize(sym->type) == 8) {
    fprintf(Outfile, "\tsd\t%s, (%s)\n", reglist[r], reglist[addrreg]);
  } else
    switch (sym->type) {
    case P_CHAR:
      fprintf(Outfile, "\tsb\t%s, (%s)\n", reglist[r], reglist[addrreg]);
      break;
    case P_INT:
      fprintf(Outfile, "\tsw\t%s, (%s)\n", reglist[r], reglist[addrreg]);
      break;
    default:
      fatald("Bad type in cgstorglob:", sym->type);
    }
  cgfreereg(addrreg);
  return (r);
}

// Store a register's value into a local variable
int cgstorlocal(int r, struct symtable *sym) {

  if (cgprimsize(sym->type) == 8) {
    fprintf(Outfile, "\tsd\t%s, %d(fp)\n", reglist[r], sym->st_posn);
  } else
    switch (sym->type) {
    case P_CHAR:
      fprintf(Outfile, "\tsb\t%s, %d(fp)\n", reglist[r], sym->st_posn);
      break;
    case P_INT:
      fprintf(Outfile, "\tsw\t%s, %d(fp)\n", reglist[r], sym->st_posn);
      break;
    default:
      fatald("Bad type in cgstorlocal:", sym->type);
    }
  return (r);
}

// Generate a global symbol but not functions
void cgglobsym(struct symtable *node) {
  int size, type;
  int initvalue;
  int i;

  if (node == NULL)
    return;
  if (node->stype == S_FUNCTION)
    return;

  // Get the size of the variable (or its elements if an array)
  // and the type of the variable
  if (node->stype == S_ARRAY) {
    size = typesize(value_at(node->type), node->ctype);
    type = value_at(node->type);
  } else {
    size = node->size;
    type = node->type;
  }

  // Generate the global identity and the label
  cgdataseg();
  if (node->class == C_GLOBAL)
    fprintf(Outfile, "\t.globl\t%s\n", node->name);
  fprintf(Outfile, "%s:\n", node->name);

  // Output space for one or more elements
  for (i = 0; i < node->nelems; i++) {

    // Get any initial value
    initvalue = 0;
    if (node->initlist != NULL)
      initvalue = node->initlist[i];

    // Generate the space for this type
    switch (size) {
    case 1:
      fprintf(Outfile, "\t.byte\t%d\n", initvalue);
      break;
    case 4:
      fprintf(Outfile, "\t.long\t%d\n", initvalue);
      break;
    case 8:
      // Generate the pointer to a string literal. Treat a zero value
      // as actually zero, not the label L0
      if (node->initlist != NULL && type == pointer_to(P_CHAR)
	  && initvalue != 0)
	fprintf(Outfile, "\t.quad\tL%d\n", initvalue);
      else
	fprintf(Outfile, "\t.quad\t%d\n", initvalue);
      break;
    default:
      for (i = 0; i < size; i++)
	fprintf(Outfile, "\t.byte\t0\n");
    }
  }
}

// Generate a global string and its start label.
// Don't output the label if append is true.
void cgglobstr(int l, char *strvalue, int append) {
  char *cptr;
  if (!append)
    cglabel(l);
  for (cptr = strvalue; *cptr; cptr++) {
    fprintf(Outfile, "\t.byte\t%d\n", *cptr);
  }
}

// NUL terminate a global string
void cgglobstrend(void) {
  fprintf(Outfile, "\t.byte\t0\n");
}

// List of comparison instructions,
// in AST order: A_EQ, A_NE, A_LT, A_GT, A_LE, A_GE
static char *cmplist[] =
  { "bne", "beq", "bge", "ble", "bgt", "blt" };

// Compare two registers and set if true.
int cgcompare_and_set(int ASTop, int r1, int r2, int type) {
  int size = cgprimsize(type);
  int resreg = cgallocreg();

  // Check the range of the AST operation
  if (ASTop < A_EQ || ASTop > A_GE)
    fatal("Bad ASTop in cgcompare_and_set()");

  switch (size) {
  case 1:
    fprintf(Outfile, "\tslli\t%s, %s, 56\n", reglist[r1], reglist[r1]);
    fprintf(Outfile, "\tsrli\t%s, %s, 56\n", reglist[r1], reglist[r1]);
    fprintf(Outfile, "\tslli\t%s, %s, 56\n", reglist[r2], reglist[r2]);
    fprintf(Outfile, "\tsrli\t%s, %s, 56\n", reglist[r2], reglist[r2]);
    break;
  case 4:
    fprintf(Outfile, "\tslli\t%s, %s, 32\n", reglist[r1], reglist[r1]);
    fprintf(Outfile, "\tsrli\t%s, %s, 32\n", reglist[r1], reglist[r1]);
    fprintf(Outfile, "\tslli\t%s, %s, 32\n", reglist[r2], reglist[r2]);
    fprintf(Outfile, "\tsrli\t%s, %s, 32\n", reglist[r2], reglist[r2]);
    break;
  default:
    break;
    // if size == 8, no mask is needed
    // fprintf(Outfile, "\tcmpq\t%s, %s\n", reglist[r2], reglist[r1]);
  }
  fprintf(Outfile, "\tli %s, 0\n", reglist[resreg]);
  fprintf(Outfile, "\t%s\t%s, %s, 1f\n", cmplist[ASTop - A_EQ], reglist[r1], reglist[r2]);
  fprintf(Outfile, "\tli %s, 1\n" "1:\n", reglist[resreg]);
  cgfreereg(r1);
  cgfreereg(r2);
  return (resreg);
}

// Generate a label
void cglabel(int l) {
  fprintf(Outfile, "L%d:\n", l);
}

// Generate a jump to a label
void cgjump(int l) {
  fprintf(Outfile, "\tj\tL%d\n", l);
}

// List of inverted jump instructions,
// in AST order: A_EQ, A_NE, A_LT, A_GT, A_LE, A_GE
static char *invcmplist[] = { "bne", "beq", "bge", "ble", "bgt", "blt" };

// Compare two registers and jump if false.
int cgcompare_and_jump(int ASTop, int r1, int r2, int label, int type) {
  int size = cgprimsize(type);

  // Check the range of the AST operation
  if (ASTop < A_EQ || ASTop > A_GE)
    fatal("Bad ASTop in cgcompare_and_set()");

  switch (size) {
  case 1:
    fprintf(Outfile, "\taddi sp, sp, -8\n" "\tsd %s, (sp)\n" "\tlb %s, (sp)\n" "\tsd %s, (sp)\n" "\tlb %s, (sp)\n" "\taddi sp, sp, 8\n", reglist[r1], reglist[r1], reglist[r2], reglist[r2]);
    break;
  case 4:
    fprintf(Outfile, "\taddi sp, sp, -8\n" "\tsd %s, (sp)\n" "\tlw %s, (sp)\n" "\tsd %s, (sp)\n" "\tlw %s, (sp)\n" "\taddi sp, sp, 8\n", reglist[r1], reglist[r1], reglist[r2], reglist[r2]);
    break;
  default:
    break;
    // if size == 8, no mask is needed
    // fprintf(Outfile, "\tcmpq\t%s, %s\n", reglist[r2], reglist[r1]);
  }

  fprintf(Outfile, "\t%s %s, %s, \tL%d\n", invcmplist[ASTop - A_EQ], reglist[r1], reglist[r2], label);
  cgfreereg(r1);
  cgfreereg(r2);
  return (NOREG);
}

// Widen the value in the register from the old
// to the new type, and return a register with
// this new value
int cgwiden(int r, int oldtype, int newtype) {
  // Nothing to do
  return (r);
}

// Generate code to return a value from a function
void cgreturn(int reg, struct symtable *sym) {

  // Only return a value if we have a value to return
  if (reg != NOREG) {
    // Deal with pointers here as we can't put them in
    // the switch statement
    if (ptrtype(sym->type))
      fprintf(Outfile, "\tadd\ta0, %s, zero\n", reglist[reg]);
    else {
      // Generate code depending on the function's type
      switch (sym->type) {
      case P_CHAR:
	fprintf(Outfile, "\tadd\ta0, %s, zero\n" "\tslli a0, a0, 56\n" "\tsrli a0, a0, 56\n", reglist[reg]);
	break;
      case P_INT:
	fprintf(Outfile, "\tadd\ta0, %s, zero\n" "\tslli a0, a0, 32\n" "\tsrli a0, a0, 32\n", reglist[reg]);
	break;
      case P_LONG:
	fprintf(Outfile, "\tadd\ta0, %s, zero\n", reglist[reg]);
	break;
      default:
	fatald("Bad function type in cgreturn:", sym->type);
      }
    }
  }

  cgjump(sym->st_endlabel);
}

// Generate code to load the address of an
// identifier into a variable. Return a new register
int cgaddress(struct symtable *sym) {
  int r = cgallocreg();

  if (sym->class == C_GLOBAL ||
      sym->class == C_EXTERN || sym->class == C_STATIC)
    fprintf(Outfile, "\tla\t%s, %s\n", reglist[r], sym->name);
  else
    fprintf(Outfile, "\taddi\t%s, fp, %d\n", reglist[r], sym->st_posn);
  return (r);
}

// Dereference a pointer to get the value
// it points at into the same register
int cgderef(int r, int type) {
  // Get the type that we are pointing to
  int newtype = value_at(type);
  // Now get the size of this type
  int size = cgprimsize(newtype);

  switch (size) {
  case 1:
    fprintf(Outfile, "\tlbu\t%s, (%s)\n", reglist[r], reglist[r]);
    break;
  case 4:
    fprintf(Outfile, "\tlw\t%s, (%s)\n", reglist[r], reglist[r]);
    break;
  case 8:
    fprintf(Outfile, "\tld\t%s, (%s)\n", reglist[r], reglist[r]);
    break;
  default:
    fatald("Can't cgderef on type:", type);
  }
  return (r);
}

// Store through a dereferenced pointer
int cgstorderef(int r1, int r2, int type) {
  // Get the size of the type
  int size = cgprimsize(type);

  switch (size) {
  case 1:
    fprintf(Outfile, "\tsb\t%s, (%s)\n", reglist[r1], reglist[r2]);
    break;
  case 4:
    fprintf(Outfile, "\tsw\t%s, (%s)\n", reglist[r1], reglist[r2]);
    break;
  case 8:
    fprintf(Outfile, "\tsd\t%s, (%s)\n", reglist[r1], reglist[r2]);
    break;
  default:
    fatald("Can't cgstoderef on type:", type);
  }
  return (r1);
}

// Generate a switch jump table and the code to
// load the registers and call the switch() code
void cgswitch(int reg, int casecount, int toplabel,
	      int *caselabel, int *caseval, int defaultlabel) {
  int i, label;

  // Get a label for the switch table
  label = genlabel();
  cglabel(label);

  // Heuristic. If we have no cases, create one case
  // which points to the default case
  if (casecount == 0) {
    caseval[0] = 0;
    caselabel[0] = defaultlabel;
    casecount = 1;
  }
  // Generate the switch jump table.
  fprintf(Outfile, "\t.quad\t%d\n", casecount);
  for (i = 0; i < casecount; i++)
    fprintf(Outfile, "\t.quad\t%d, L%d\n", caseval[i], caselabel[i]);
  fprintf(Outfile, "\t.quad\tL%d\n", defaultlabel);

  // Load the specific registers
  cglabel(toplabel);
  fprintf(Outfile, "\tadd\ta0, %s, zero\n", reglist[reg]);
  fprintf(Outfile, "\tla\ta1, L%d\n", label);
  fprintf(Outfile, "\tcall\t__switch\n");
}

// Move value between registers
void cgmove(int r1, int r2) {
  fprintf(Outfile, "\tadd\t%s, %s, zero\n", reglist[r2], reglist[r1]);
}

// Output a gdb directive to say on which
// source code line number the following
// assembly code came from
void cglinenum(int line) {
  // fprintf(Outfile, "\t.loc 1 %d 0\n", line);
}
