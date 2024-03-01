// RUN: llvm-mc --triple=loongarch64 --filetype=obj -o %t.o %s
// RUN: ld.lld --emit-relocs -o %t %t.o
// RUN: llvm-bolt --print-cfg --print-only=tls_le,tls_ie -o %t.null %t \
// RUN:    | FileCheck %s

// CHECK-LABEL: Binary Function "tls_le{{.*}}" after building cfg {
// CHECK:      lu12i.w $a0, 0
// CHECK-NEXT: ori $a0, $a0, 0

// CHECK-LABEL: Binary Function "tls_ie" after building cfg {
// CHECK-LABEL: .LBB01
// CHECK:      pcalau12i $a0, 16
// CHECK-NEXT: ld.d $a0, $a0, 488
    .text
    .globl tls_le, _start
    .p2align 2
tls_le:
_start:
    nop
    lu12i.w $a0, %le_hi20(i)
    ori  $a0, $a0, %le_lo12(i)
    ret
    .size _start, .-_start

    .globl tls_ie
    .p2align 2
tls_ie:
    nop
    la.tls.ie $a0, i
    ret
    .size tls_ie, .-tls_ie

    .section .tbss,"awT",@nobits
    .type i,@object
    .globl i
    .p2align 3
i:
    .quad 0
    .size i, .-i
