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

LoongArchCallLowering::LoongArchCallLowering(const LoongArchTargetLowering &TLI)
    : CallLowering(&TLI) {}

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

namespace {

struct LoongArchIncomingValueAssigner
    : public CallLowering::IncomingValueAssigner {
private:
  // The function used internally to assign args - we ignore the AssignFn stored
  // by IncomingValueAssigner since LoongArch implements its CC using a custom
  // function with a different signature.
  LoongArchTargetLowering::LoongArchCCAssignFn *LoongArchAssignFn;

  // Whether this is assigning args from a return.
  bool IsRet;

public:
  LoongArchIncomingValueAssigner(
      LoongArchTargetLowering::LoongArchCCAssignFn *LoongArchAssignFn_,
      bool IsRet)
      : CallLowering::IncomingValueAssigner(nullptr),
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

struct LoongArchIncomingValueHandler
    : public CallLowering::IncomingValueHandler {
  LoongArchIncomingValueHandler(MachineIRBuilder &B, MachineRegisterInfo &MRI)
      : IncomingValueHandler(B, MRI) {}

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
    // Copy argument received in physical register to desired VReg.
    MIRBuilder.getMBB().addLiveIn(PhysReg);
    MIRBuilder.buildCopy(ValVReg, PhysReg);
  }
};

} // namespace

bool LoongArchCallLowering::lowerFormalArguments(
    MachineIRBuilder &MIRBuilder, const Function &F,
    ArrayRef<ArrayRef<Register>> VRegs, FunctionLoweringInfo &FLI) const {
  // Early exit if there are no arguments.
  if (F.arg_empty())
    return true;

  // TODO: Support vararg functions.
  if (F.isVarArg())
    return false;

  // TODO: Support all argument types.
  for (auto &Arg : F.args()) {
    if (Arg.getType()->isIntOrPtrTy())
      continue;
    return false;
  }

  MachineFunction &MF = MIRBuilder.getMF();
  const DataLayout &DL = MF.getDataLayout();
  CallingConv::ID CC = F.getCallingConv();

  SmallVector<ArgInfo, 32> SplitArgInfos;
  unsigned Index = 0;
  for (auto &Arg : F.args()) {
    // Construct the ArgInfo object from destination register and argument type.
    ArgInfo AInfo(VRegs[Index], Arg.getType(), Index);
    setArgFlags(AInfo, Index + AttributeList::FirstArgIndex, DL, F);

    // Handle any required merging from split value types from physical
    // registers into the desired VReg. ArgInfo objects are constructed
    // correspondingly and appended to SplitArgInfos.
    splitToValueTypes(AInfo, SplitArgInfos, DL, CC);

    ++Index;
  }

  LoongArchIncomingValueAssigner Assigner(LoongArch::CC_LoongArch,
                                          /*IsRet=*/false);
  LoongArchIncomingValueHandler Handler(MIRBuilder, MF.getRegInfo());

  return determineAndHandleAssignments(Handler, Assigner, SplitArgInfos,
                                       MIRBuilder, CC, F.isVarArg());
}

bool LoongArchCallLowering::lowerCall(MachineIRBuilder &MIRBuilder,
                                      CallLoweringInfo &Info) const {
  return false;
}
