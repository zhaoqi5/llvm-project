; RUN: llc --mtriple=loongarch64 -mattr=+d,+relax --stop-after loongarch-prera-expand-pseudo \
; RUN:     --relocation-model=pic --code-model=medium %s -o %t.mir
; RUN: llc --mtriple=loongarch64 -mattr=+d,+relax --stop-before loongarch-expand-pseudo \
; RUN:     --code-model=medium %t.mir -o - | FileCheck %s --check-prefixes=BEFORE
; RUN: llc --mtriple=loongarch64 -mattr=+d,+relax --run-pass loongarch-expand-pseudo \
; RUN:     --code-model=medium %t.mir -o - | FileCheck %s --check-prefixes=AFTER

; RUN: llc --mtriple=loongarch64 -mattr=+d,+relax --stop-after loongarch-prera-expand-pseudo \
; RUN:     --relocation-model=pic --enable-tlsdesc --code-model=medium %s -o %t.desc.mir
; RUN: llc --mtriple=loongarch64 -mattr=+d,+relax --stop-before loongarch-expand-pseudo \
; RUN:     --code-model=medium %t.desc.mir -o - | FileCheck %s --check-prefixes=DESC
; RUN: llc --mtriple=loongarch64 -mattr=+d,+relax --run-pass loongarch-expand-pseudo \
; RUN:     --code-model=medium %t.desc.mir -o - | FileCheck %s --check-prefixes=DESCAFTER

;; Check Pseudos/instructions/target-flags before and after loongarch-expand-pseudo pass.

@g_e = external global i32
@g_i = internal global i32 0
@t_un = external thread_local global i32
@t_ld = external thread_local(localdynamic) global i32
@t_ie = external thread_local(initialexec) global i32
@t_le = external thread_local(localexec) global i32

declare void @callee1() nounwind
declare dso_local void @callee2() nounwind

define void @caller() nounwind {
; BEFORE:         PseudoLA_GOT @g_e
; AFTER:          target-flags(loongarch-got-pc-hi, loongarch-relax) @g_e
; AFTER-NEXT:     target-flags(loongarch-got-pc-lo, loongarch-relax) @g_e
; BEFORE:         PseudoLA_PCREL @g_i
; AFTER:          target-flags(loongarch-pcrel-hi, loongarch-relax) @g_i
; AFTER-NEXT:     target-flags(loongarch-pcrel-lo, loongarch-relax) @g_i
; BEFORE:         PseudoLA_TLS_GD @t_un
; AFTER:          target-flags(loongarch-gd-pc-hi, loongarch-relax) @t_un
; AFTER-NEXT:     target-flags(loongarch-got-pc-lo, loongarch-relax) @t_un
; DESC:           PseudoLA_TLS_DESC @t_un
; DESCAFTER:      target-flags(loongarch-desc-pc-hi, loongarch-relax) @t_un
; DESCAFTER-NEXT: target-flags(loongarch-desc-pc-lo, loongarch-relax) @t_un
; DESCAFTER-NEXT: target-flags(loongarch-desc-ld, loongarch-relax) @t_un
; DESCAFTER-NEXT: target-flags(loongarch-desc-call, loongarch-relax) @t_un
; BEFORE:         PseudoLA_TLS_LD @t_ld
; AFTER:          target-flags(loongarch-ld-pc-hi, loongarch-relax) @t_ld
; AFTER-NEXT:     target-flags(loongarch-got-pc-lo, loongarch-relax) @t_ld
; DESC:           PseudoLA_TLS_DESC @t_ld
; DESCAFTER:      target-flags(loongarch-desc-pc-hi, loongarch-relax) @t_ld
; DESCAFTER-NEXT: target-flags(loongarch-desc-pc-lo, loongarch-relax) @t_ld
; DESCAFTER-NEXT: target-flags(loongarch-desc-ld, loongarch-relax) @t_ld
; DESCAFTER-NEXT: target-flags(loongarch-desc-call, loongarch-relax) @t_ld
; BEFORE:         PseudoLA_TLS_IE @t_ie
; AFTER:          target-flags(loongarch-ie-pc-hi, loongarch-relax) @t_ie
; AFTER-NEXT:     target-flags(loongarch-ie-pc-lo, loongarch-relax) @t_ie
; BEFORE:         target-flags(loongarch-le-hi-r, loongarch-relax) @t_le
; BEFORE-NEXT:    target-flags(loongarch-le-add-r, loongarch-relax) @t_le
; BEFORE-NEXT:    target-flags(loongarch-le-lo-r, loongarch-relax) @t_le
; BEFORE:         target-flags(loongarch-call-plt) @callee1
; AFTER:          target-flags(loongarch-call36, loongarch-relax) @callee1
; BEFORE:         target-flags(loongarch-call) @callee2
; AFTER:          target-flags(loongarch-call36, loongarch-relax) @callee2
  %a = load volatile i32, ptr @g_e
  %b = load volatile i32, ptr @g_i
  %c = load volatile i32, ptr @t_un
  %d = load volatile i32, ptr @t_ld
  %e = load volatile i32, ptr @t_ie
  %f = load volatile i32, ptr @t_le
  call i32 @callee1()
  call i32 @callee2()
  ret void
}
