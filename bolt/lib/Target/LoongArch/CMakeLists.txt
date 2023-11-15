set(LLVM_LINK_COMPONENTS
  MC
  Support
  LoongArchDesc
  )

add_llvm_library(LLVMBOLTTargetLoongArch
  LoongArchMCPlusBuilder.cpp

  DISABLE_LLVM_LINK_LLVM_DYLIB

  DEPENDS
  LoongArchCommonTableGen
  )

target_link_libraries(LLVMBOLTTargetLoongArch PRIVATE LLVMBOLTCore)

include_directories(
  ${LLVM_MAIN_SRC_DIR}/lib/Target/LoongArch
  ${LLVM_BINARY_DIR}/lib/Target/LoongArch
  )
