//===-- LoongArchCallLowering.cpp - Call lowering ---------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
/// This file implements the lowering of LLVM calls to machine code calls for
/// GlobalISel.
//
//===----------------------------------------------------------------------===//

#include "LoongArchCallLowering.h"
#include "LoongArchISelLowering.h"
#include "LoongArchSubtarget.h"
#include "llvm/CodeGen/Analysis.h"
#include "llvm/CodeGen/GlobalISel/MachineIRBuilder.h"

using namespace llvm;

namespace {

struct LoongArchOutgoingValueAssigner
    : public CallLowering::OutgoingValueAssigner {
private:
  // The function used internally to assign args - we ignore the AssignFn stored
  // by OutgoingValueAssigner since LoongArch implements its CC using a custom
  // function with a different signature.
  LoongArchTargetLowering::LoongArchCCAssignFn *LoongArchAssignFn;

  // Whether this is assigning args for a return.
  bool IsRet;

public:
  LoongArchOutgoingValueAssigner(
      LoongArchTargetLowering::LoongArchCCAssignFn *LoongArchAssignFn_,
      bool IsRet)
      : CallLowering::OutgoingValueAssigner(nullptr),
        LoongArchAssignFn(LoongArchAssignFn_), IsRet(IsRet) {}

  bool assignArg(unsigned ValNo, EVT OrigVT, MVT ValVT, MVT LocVT,
                 CCValAssign::LocInfo LocInfo,
                 const CallLowering::ArgInfo &Info, ISD::ArgFlagsTy Flags,
                 CCState &State) override {
    MachineFunction &MF = State.getMachineFunction();
    const DataLayout &DL = MF.getDataLayout();
    const LoongArchSubtarget &Subtarget = MF.getSubtarget<LoongArchSubtarget>();

    return LoongArchAssignFn(DL, Subtarget.getTargetABI(), ValNo, ValVT,
                             LocInfo, Flags, State, /*IsFixed=*/true, IsRet,
                             Info.Ty);
  }
};

struct LoongArchOutgoingValueHandler
    : public CallLowering::OutgoingValueHandler {
  LoongArchOutgoingValueHandler(MachineIRBuilder &B, MachineRegisterInfo &MRI,
                                MachineInstrBuilder MIB)
      : OutgoingValueHandler(B, MRI), MIB(MIB) {}

  Register getStackAddress(uint64_t MemSize, int64_t Offset,
                           MachinePointerInfo &MPO,
                           ISD::ArgFlagsTy Flags) override {
    llvm_unreachable("not implemented");
  }

  void assignValueToAddress(Register ValVReg, Register Addr, LLT MemTy,
                            const MachinePointerInfo &MPO,
                            const CCValAssign &VA) override {
    llvm_unreachable("not implemented");
  }

  void assignValueToReg(Register ValVReg, Register PhysReg,
                        const CCValAssign &VA) override {
    Register ExtReg = extendRegister(ValVReg, VA);
    MIRBuilder.buildCopy(PhysReg, ExtReg);
    MIB.addUse(PhysReg, RegState::Implicit);
  }

private:
  MachineInstrBuilder MIB;
};

} // namespace

LoongArchCallLowering::LoongArchCallLowering(const LoongArchTargetLowering &TLI)
    : CallLowering(&TLI) {}

bool LoongArchCallLowering::lowerReturn(MachineIRBuilder &MIRBuilder,
                                        const Value *Val,
                                        ArrayRef<Register> VRegs,
                                        FunctionLoweringInfo &FLI) const {
  assert(!Val == VRegs.empty() && "Return value without a vreg");
  MachineInstrBuilder Ret = MIRBuilder.buildInstrNoInsert(LoongArch::PseudoRET);

  if (Val != nullptr) {
    // TODO: Only integer, pointer and aggregate types are supported now.
    if (!Val->getType()->isIntOrPtrTy() && !Val->getType()->isAggregateType())
      return false;

    MachineFunction &MF = MIRBuilder.getMF();
    const DataLayout &DL = MF.getDataLayout();
    const Function &F = MF.getFunction();
    CallingConv::ID CC = F.getCallingConv();

    ArgInfo OrigRetInfo(VRegs, Val->getType(), 0);
    setArgFlags(OrigRetInfo, AttributeList::ReturnIndex, DL, F);

    SmallVector<ArgInfo, 4> SplitRetInfos;
    splitToValueTypes(OrigRetInfo, SplitRetInfos, DL, CC);

    LoongArchOutgoingValueAssigner Assigner(LoongArch::CC_LoongArch,
                                            /*IsRet=*/true);
    LoongArchOutgoingValueHandler Handler(MIRBuilder, MF.getRegInfo(), Ret);
    if (!determineAndHandleAssignments(Handler, Assigner, SplitRetInfos,
                                       MIRBuilder, CC, F.isVarArg()))
      return false;
  }

  MIRBuilder.insertInstr(Ret);
  return true;
}

bool LoongArchCallLowering::lowerFormalArguments(
    MachineIRBuilder &MIRBuilder, const Function &F,
    ArrayRef<ArrayRef<Register>> VRegs, FunctionLoweringInfo &FLI) const {
  if (F.arg_empty())
    return true;

  return false;
}

bool LoongArchCallLowering::lowerCall(MachineIRBuilder &MIRBuilder,
                                      CallLoweringInfo &Info) const {
  return false;
}
