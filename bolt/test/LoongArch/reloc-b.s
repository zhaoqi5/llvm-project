// RUN: %clang %cflags -o %t %s
// RUN: llvm-bolt --print-cfg --print-fix-loongarch-calls --print-only=_start \
// RUN:     -o %t.bolt %t | FileCheck %s
// RUN: llvm-objdump -d %t.bolt | FileCheck --check-prefix=OBJDUMP %s

// CHECK:      Binary Function "_start" after building cfg {
// CHECK:      b .Ltmp0
// CHECK:      b f # TAILCALL

// CHECK:      Binary Function "_start" after fix-loongarch-calls {
// CHECK:      b .Ltmp0
// CHECK:      pcaddu18i $t8, %call36(f)
// CHECK-NEXT: jirl $zero, $t8, 0 # TAILCALL

// OBJDUMP:      0000000000400000 <f>:
// OBJDUMP:      0000000000400004 <_start>:
// OBJDUMP-NEXT:     pcaddu18i $t8, 0
// OBJDUMP-NEXT:     jirl $zero, $t8, -4

  .text

  .global f
  .p2align 1
f:
  ret
  .size f, .-f

  .globl _start
  .p2align 1
_start:
  b 1f
1:
  b f
  ret
  .size _start, .-_start
