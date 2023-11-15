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
// CHECK: pcalau12i $t0, %got_pc_hi20(__BOLT_got_zero+{{[0-9]+}})
// CHECK-NEXT: ld.d $t0, $a2, %got_pc_lo12(__BOLT_got_zero+{{[0-9]+}})
  pcalau12i $t0, %got_pc_hi20(f)
  ld.d $t0, $a2, %got_pc_lo12(f)
  ret
  .size _start, .-_start
