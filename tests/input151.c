
asm(".global main\n" "main:\n" "li a0, 1\n" "la a1, str\n" "li a7, 64\n" "li a2, 1\n" "ecall\n" "ret\n"  "str: .byte 65\n.byte 0\n");
