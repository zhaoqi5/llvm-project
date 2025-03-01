; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=riscv64 -mattr=+v < %s | FileCheck %s

@c = global [7 x i64] [i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, i64 7], align 8

define void @do.memmove() nounwind {
; CHECK-LABEL: do.memmove:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    lui a0, %hi(c)
; CHECK-NEXT:    addi a0, a0, %lo(c)
; CHECK-NEXT:    addi a1, a0, 24
; CHECK-NEXT:    addi a2, a0, 16
; CHECK-NEXT:    vsetivli zero, 2, e64, m1, ta, ma
; CHECK-NEXT:    vle64.v v8, (a2)
; CHECK-NEXT:    vse64.v v8, (a1)
; CHECK-NEXT:    vle64.v v8, (a0)
; CHECK-NEXT:    addi a0, a0, 8
; CHECK-NEXT:    vse64.v v8, (a0)
; CHECK-NEXT:    ret
entry:
  ; this thing is "__builtin_memmove(&c[1], &c[0], sizeof(c[0]) * 4);"
  tail call void @llvm.memmove.p0.p0.i64(
        ptr noundef nonnull align 8 dereferenceable(32) getelementptr inbounds ([7 x i64], ptr @c, i64 0, i64 1),
        ptr noundef nonnull align 8 dereferenceable(32) @c, i64 32, i1 false)
  ret void
}

; Function Attrs: argmemonly mustprogress nofree nounwind willreturn
declare void @llvm.memmove.p0.p0.i64(ptr nocapture writeonly, ptr nocapture readonly, i64, i1 immarg) #1

attributes #1 = { argmemonly mustprogress nofree nounwind willreturn }
