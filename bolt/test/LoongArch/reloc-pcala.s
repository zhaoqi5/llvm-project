// RUN: %clang %cflags -o %t %s
// RUN: llvm-bolt --print-cfg --print-only=_start -o %t.null %t \
// RUN:    | FileCheck %s

  .text

  .global f
  .p2align 1
f:
  ret
  .size f, .-f

  .globl _start
  .p2align 1
// CHECK: Binary Function "_start" after building cfg {
_start:
// CHECK: pcalau12i $t1, %pc_hi20(f)
// CHECK-NEXT: addi.d $t1, $t1, %pc_lo12(f)
  pcalau12i $t1, %pc_hi20(f)
  addi.d $t1, $t1, %pc_lo12(f)
  ret
  .size _start, .-_start
