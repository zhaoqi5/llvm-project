// RUN: %clang %cflags -o %t %s
// RUN: llvm-bolt --print-cfg --print-fix-loongarch-calls --print-only=_start \
// RUN:     -o %t.bolt %t | FileCheck %s
// RUN: llvm-objdump -d %t.bolt | FileCheck --check-prefix=OBJDUMP %s

// CHECK:      Binary Function "_start" after building cfg {
// CHECK:      bl f

// CHECK:      Binary Function "_start" after fix-loongarch-calls {
// CHECK:      pcaddu18i $ra, %call36(f)
// CHECK-NEXT: jirl $ra, $ra, 0

// OBJDUMP:      0000000000400000 <f>:
// OBJDUMP:      0000000000400004 <_start>:
// OBJDUMP-NEXT:     pcaddu18i $ra, 0
// OBJDUMP-NEXT:     jirl $ra, $ra, -4

/// Just provide a near call via bl, because the fix-loongarch-calls pass
/// expand all bls whether they are near calls or far calls. To show a far
/// call, a large yaml file contains calls to .plt section maybe needed.
/// So drop it. This test can show the same effect.

  .text

  .global f
  .p2align 1
f:
  ret
  .size f, .-f

  .globl _start
  .p2align 1
_start:
  bl f
  ret
  .size _start, .-_start
