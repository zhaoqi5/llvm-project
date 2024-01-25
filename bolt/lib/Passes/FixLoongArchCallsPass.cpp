#include "bolt/Passes/FixLoongArchCallsPass.h"
#include "bolt/Core/ParallelUtilities.h"

#include <iterator>

using namespace llvm;

namespace llvm {
namespace bolt {

void FixLoongArchCallsPass::runOnFunction(BinaryFunction &BF) {
  auto &BC = BF.getBinaryContext();
  auto &MIB = BC.MIB;
  auto *Ctx = BC.Ctx.get();

  for (auto &BB : BF) {
    for (auto II = BB.begin(); II != BB.end(); ++II) {
      if (MIB->isCall(*II) && !MIB->isIndirectCall(*II)) {
        auto *Target = MIB->getTargetSymbol(*II);
        assert(Target && "Cannot find call target");

        MCInst OldCall = *II;
        auto L = BC.scopeLock();

        MCInst InsertII;
        MIB->createLoongArchCall(*II, InsertII, Target, Ctx,
                                 MIB->isTailCall(*II));
        II = BB.insertInstruction(std::next(II), std::move(InsertII));

        MIB->moveAnnotations(std::move(OldCall), *II);
      }
    }
  }
}

Error FixLoongArchCallsPass::runOnFunctions(BinaryContext &BC) {
  if (!BC.isLoongArch() || !BC.HasRelocations)
    return Error::success();

  ParallelUtilities::WorkFuncTy WorkFun = [&](BinaryFunction &BF) {
    runOnFunction(BF);
  };

  ParallelUtilities::runOnEachFunction(
      BC, ParallelUtilities::SchedulingPolicy::SP_INST_LINEAR, WorkFun, nullptr,
      "FixLoongArchCalls");

  return Error::success();
}

} // namespace bolt
} // namespace llvm
