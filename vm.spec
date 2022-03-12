LIT $REG $LIT
LOD $REG $MEM
STO $REG $MEM

ADD $REG $REG
SUB $REG $REG
MUL $REG $REG
DIV $REG $REG

CMP $REG $REG
JMP $INT
JZE $INT
JLE $INT
JLT $INT

SYS $CMD $MEM

A Virtual Machine has a code store which can be jumped around inside, a stack
for memory, and 32 registers. System calls calls can be used for input and
output.
