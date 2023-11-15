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
// CHECK: lu32i.d $t1, %got64_pc_lo20(__BOLT_got_zero)
// CHECK-NEXT: lu52i.d $t1, $t1, %got64_pc_hi12(__BOLT_got_zero)
  lu32i.d $t1, %got64_pc_lo20(f)
  lu52i.d $t1, $t1, %got64_pc_hi12(f)
  ret
  .size _start, .-_start
