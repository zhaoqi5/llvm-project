# RUN: llvm-mc --triple=loongarch64 %s --show-encoding \
# RUN:     | FileCheck --check-prefix=CHECK-FIXUP %s
# RUN: llvm-mc --filetype=obj --triple=loongarch64 -mattr=-relax %s \
# RUN:     | llvm-objdump -dr - | FileCheck --check-prefix=NORELAX %s
# RUN: llvm-mc --filetype=obj --triple=loongarch64 -mattr=+relax %s \
# RUN:     | llvm-objdump -dr - | FileCheck --check-prefix=RELAX %s

## Testing the function call offset could resolved by assembler
## when the function and the callsite within the same compile unit
## and the linker relaxation is disabled.

func:
.fill 100
call36 func
# CHECK-FIXUP:  fixup A - offset: 0, value: %call36(func), kind: fixup_loongarch_call36
# NORELAX:      pcaddu18i $ra, 0
# NORELAX-NEXT: jirl $ra, $ra, -100 <func>
# RELAX:        pcaddu18i $ra, 0
# RELAX-NEXT:     R_LARCH_CALL36 func
# RELAX-NEXT:     R_LARCH_RELAX *ABS*
# RELAX-NEXT:   jirl $ra, $ra, 0

call36 foo
# CHECK-FIXUP:  fixup A - offset: 0, value: %call36(foo), kind: fixup_loongarch_call36
# NORELAX:      pcaddu18i $ra, 0
# NORELAX-NEXT:   R_LARCH_CALL36 foo
# NORELAX-NEXT: jirl $ra, $ra, 0
# RELAX:        pcaddu18i $ra, 0
# RELAX-NEXT:     R_LARCH_CALL36 foo
# RELAX-NEXT:     R_LARCH_RELAX *ABS*
# RELAX-NEXT:   jirl $ra, $ra, 0

.fill 1000000
call36 func
# CHECK-FIXUP:  fixup A - offset: 0, value: %call36(func), kind: fixup_loongarch_call36
# NORELAX:      pcaddu18i $ra, -4
# NORELAX-NEXT: jirl $ra, $ra, 48460 <func>
# RELAX:        pcaddu18i $ra, 0
# RELAX-NEXT:     R_LARCH_CALL36 func
# RELAX-NEXT:     R_LARCH_RELAX *ABS*
# RELAX-NEXT:   jirl $ra, $ra, 0

call36 foo
# CHECK-FIXUP:  fixup A - offset: 0, value: %call36(foo), kind: fixup_loongarch_call36
# NORELAX:      pcaddu18i $ra, 0
# NORELAX-NEXT:   R_LARCH_CALL36 foo
# NORELAX-NEXT: jirl $ra, $ra, 0
# RELAX:        pcaddu18i $ra, 0
# RELAX-NEXT:     R_LARCH_CALL36 foo
# RELAX-NEXT:     R_LARCH_RELAX *ABS*
# RELAX-NEXT:   jirl $ra, $ra, 0
