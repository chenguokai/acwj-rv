asm(".global _start\n" ".balign 4\n" "_start:\n");
asm("\tlui sp, %hi(0x80040000)\n"
    "\taddi sp, sp, %lo(0x80040000)\n"
    "\tslli sp, sp, 32\n" 
    "\tsrli sp, sp, 32\n" // set up stack pointer
    "\tla gp, __global_pointer$\n" // set up global pointer
    ".macro SBI_CALL which\n" // sbi call asm wrapper
    "\tli a7, \\which\n"
    "\tecall\n"
    "\t.endm\n"
    "\taddi a0, zero, 48\n" // putchar '0'
    "\tSBI_CALL 1\n"
    "\tcall main\n"
);

asm(".global sbi_putchar\n"
    "sbi_putchar:\n"
    "\tSBI_CALL 1\n"
    "\tret\n"
);

void sbi_putchar();

int main() {
	sbi_putchar('4');
	sbi_putchar('\n');
	while (1); // go to dead loop
    return (0); // make compiler happy
}
