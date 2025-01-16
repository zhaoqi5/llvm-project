define <8 x i32> @trunc2x4i64_8i32(<4 x i64> %a, <4 x i64> %b) {
; SSE-LABEL: trunc2x4i64_8i32:
; SSE:       # %bb.0: # %entry
; SSE-NEXT:    shufps {{.*#+}} xmm0 = xmm0[0,2],xmm1[0,2]
; SSE-NEXT:    shufps {{.*#+}} xmm2 = xmm2[0,2],xmm3[0,2]
; SSE-NEXT:    movaps %xmm2, %xmm1
; SSE-NEXT:    retq
;
; AVX1-LABEL: trunc2x4i64_8i32:
; AVX1:       # %bb.0: # %entry
; AVX1-NEXT:    vperm2f128 {{.*#+}} ymm2 = ymm0[2,3],ymm1[2,3]
; AVX1-NEXT:    vinsertf128 $1, %xmm1, %ymm0, %ymm0
; AVX1-NEXT:    vshufps {{.*#+}} ymm0 = ymm0[0,2],ymm2[0,2],ymm0[4,6],ymm2[4,6]
; AVX1-NEXT:    retq
;
; AVX2-SLOW-LABEL: trunc2x4i64_8i32:
; AVX2-SLOW:       # %bb.0: # %entry
; AVX2-SLOW-NEXT:    vperm2f128 {{.*#+}} ymm2 = ymm0[2,3],ymm1[2,3]
; AVX2-SLOW-NEXT:    vinsertf128 $1, %xmm1, %ymm0, %ymm0
; AVX2-SLOW-NEXT:    vshufps {{.*#+}} ymm0 = ymm0[0,2],ymm2[0,2],ymm0[4,6],ymm2[4,6]
; AVX2-SLOW-NEXT:    retq
;
; AVX2-FAST-ALL-LABEL: trunc2x4i64_8i32:
; AVX2-FAST-ALL:       # %bb.0: # %entry
; AVX2-FAST-ALL-NEXT:    vmovaps {{.*#+}} ymm2 = [0,2,4,6,4,6,6,7]
; AVX2-FAST-ALL-NEXT:    vpermps %ymm0, %ymm2, %ymm0
; AVX2-FAST-ALL-NEXT:    vpermps %ymm1, %ymm2, %ymm1
; AVX2-FAST-ALL-NEXT:    vinsertf128 $1, %xmm1, %ymm0, %ymm0
; AVX2-FAST-ALL-NEXT:    retq
;
; AVX2-FAST-PERLANE-LABEL: trunc2x4i64_8i32:
; AVX2-FAST-PERLANE:       # %bb.0: # %entry
; AVX2-FAST-PERLANE-NEXT:    vperm2f128 {{.*#+}} ymm2 = ymm0[2,3],ymm1[2,3]
; AVX2-FAST-PERLANE-NEXT:    vinsertf128 $1, %xmm1, %ymm0, %ymm0
; AVX2-FAST-PERLANE-NEXT:    vshufps {{.*#+}} ymm0 = ymm0[0,2],ymm2[0,2],ymm0[4,6],ymm2[4,6]
; AVX2-FAST-PERLANE-NEXT:    retq
;
; AVX512-LABEL: trunc2x4i64_8i32:
; AVX512:       # %bb.0: # %entry
; AVX512-NEXT:    # kill: def $ymm0 killed $ymm0 def $zmm0
; AVX512-NEXT:    vinserti64x4 $1, %ymm1, %zmm0, %zmm0
; AVX512-NEXT:    vpmovqd %zmm0, %ymm0
; AVX512-NEXT:    retq
entry:
  %0 = trunc <4 x i64> %a to <4 x i32>
  %1 = trunc <4 x i64> %b to <4 x i32>
  %2 = shufflevector <4 x i32> %0, <4 x i32> %1, <8 x i32> <i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7>
  ret <8 x i32> %2
}
