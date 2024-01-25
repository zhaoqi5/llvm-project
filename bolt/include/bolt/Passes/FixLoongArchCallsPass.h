//===- bolt/Passes/FixLoongArchCallsPass.h ----------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares the FixLoongArchCallsPass class, which replaces all bl
// calls with pcaddu18i+jirl instructions. Without this pass, an out of range
// error will occur in some cases (eg. call .plt section via bl instr). Because
// BOLT may place the new .text section very far from the call destination of bl
// instr. This pass ensures BOLT freely reassign function addresses to a certain
// extent without having to worry about this.
//
//===----------------------------------------------------------------------===//

#ifndef BOLT_PASSES_FIXLOONGARCHCALLSPASS_H
#define BOLT_PASSES_FIXLOONGARCHCALLSPASS_H

#include "bolt/Passes/BinaryPasses.h"

namespace llvm {
namespace bolt {

class FixLoongArchCallsPass : public BinaryFunctionPass {
  void runOnFunction(BinaryFunction &Function);

public:
  explicit FixLoongArchCallsPass(const cl::opt<bool> &PrintPass)
      : BinaryFunctionPass(PrintPass) {}

  const char *getName() const override { return "fix-loongarch-calls"; }

  /// Pass entry point
  Error runOnFunctions(BinaryContext &BC) override;
};

} // namespace bolt
} // namespace llvm

#endif // BOLT_PASSES_FIXLOONGARCHCALLSPASS_H
