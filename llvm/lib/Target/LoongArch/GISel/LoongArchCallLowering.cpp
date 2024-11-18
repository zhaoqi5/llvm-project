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
#include "llvm/CodeGen/MachineFrameInfo.h"

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
      : OutgoingValueHandler(B, MRI), MIB(MIB),
        Subtarget(MIRBuilder.getMF().getSubtarget<LoongArchSubtarget>()) {}

  Register getStackAddress(uint64_t MemSize, int64_t Offset,
                           MachinePointerInfo &MPO,
                           ISD::ArgFlagsTy Flags) override {
    MachineFunction &MF = MIRBuilder.getMF();
    LLT p0 = LLT::pointer(0, Subtarget.getGRLen());
    LLT sGRLen = LLT::scalar(Subtarget.getGRLen());

    if (!SPReg)
      SPReg = MIRBuilder.buildCopy(p0, Register(LoongArch::R3)).getReg(0);

    auto OffsetReg = MIRBuilder.buildConstant(sGRLen, Offset);

    auto AddrReg = MIRBuilder.buildPtrAdd(p0, SPReg, OffsetReg);

    MPO = MachinePointerInfo::getStack(MF, Offset);
    return AddrReg.getReg(0);
  }

  void assignValueToAddress(Register ValVReg, Register Addr, LLT MemTy,
                            const MachinePointerInfo &MPO,
                            const CCValAssign &VA) override {
    MachineFunction &MF = MIRBuilder.getMF();
    uint64_t LocMemOffset = VA.getLocMemOffset();

    // Upon procedure entry, the stack pointer is required to be divisible
    // by 16, ensuring a 16-byte alignment of the frame.
    auto MMO =
        MF.getMachineMemOperand(MPO, MachineMemOperand::MOStore, MemTy,
                                commonAlignment(Align(16), LocMemOffset));

    Register ExtReg = extendRegister(ValVReg, VA);
    MIRBuilder.buildStore(ExtReg, Addr, *MMO);
  }

  void assignValueToReg(Register ValVReg, Register PhysReg,
                        const CCValAssign &VA) override {
    Register ExtReg = extendRegister(ValVReg, VA);
    MIRBuilder.buildCopy(PhysReg, ExtReg);
    MIB.addUse(PhysReg, RegState::Implicit);
  }

private:
  MachineInstrBuilder MIB;

  // Cache the SP register vreg if we need it more than once in this call site.
  Register SPReg;

  const LoongArchSubtarget &Subtarget;
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
      : IncomingValueHandler(B, MRI),
        Subtarget(MIRBuilder.getMF().getSubtarget<LoongArchSubtarget>()) {}

  Register getStackAddress(uint64_t MemSize, int64_t Offset,
                           MachinePointerInfo &MPO,
                           ISD::ArgFlagsTy Flags) override {
    MachineFrameInfo &MFI = MIRBuilder.getMF().getFrameInfo();

    int FI = MFI.CreateFixedObject(MemSize, Offset, /*Immutable=*/true);
    MPO = MachinePointerInfo::getFixedStack(MIRBuilder.getMF(), FI);
    return MIRBuilder.buildFrameIndex(LLT::pointer(0, Subtarget.getGRLen()), FI)
        .getReg(0);
  }

  void assignValueToAddress(Register ValVReg, Register Addr, LLT MemTy,
                            const MachinePointerInfo &MPO,
                            const CCValAssign &VA) override {
    MachineFunction &MF = MIRBuilder.getMF();
    auto MMO = MF.getMachineMemOperand(MPO, MachineMemOperand::MOLoad, MemTy,
                                       inferAlignFromPtrInfo(MF, MPO));
    MIRBuilder.buildLoad(ValVReg, Addr, *MMO);
  }

  void assignValueToReg(Register ValVReg, Register PhysReg,
                        const CCValAssign &VA) override {
    markPhysRegUsed(PhysReg);
    IncomingValueHandler::assignValueToReg(ValVReg, PhysReg, VA);
  }

  /// How the physical register gets marked varies between formal
  /// parameters (it's a basic-block live-in), and a call instruction
  /// (it's an implicit-def of the BL).
  virtual void markPhysRegUsed(MCRegister PhysReg) = 0;

private:
  const LoongArchSubtarget &Subtarget;
};

struct LoongArchFormalArgHandler : public LoongArchIncomingValueHandler {
  LoongArchFormalArgHandler(MachineIRBuilder &B, MachineRegisterInfo &MRI)
      : LoongArchIncomingValueHandler(B, MRI) {}

  void markPhysRegUsed(MCRegister PhysReg) override {
    MIRBuilder.getMRI()->addLiveIn(PhysReg);
    MIRBuilder.getMBB().addLiveIn(PhysReg);
  }
};

struct LoongArchCallReturnHandler : public LoongArchIncomingValueHandler {
  LoongArchCallReturnHandler(MachineIRBuilder &B, MachineRegisterInfo &MRI,
                             MachineInstrBuilder &MIB)
      : LoongArchIncomingValueHandler(B, MRI), MIB(MIB) {}

  void markPhysRegUsed(MCRegister PhysReg) override {
    MIB.addDef(PhysReg, RegState::Implicit);
  }

private:
  MachineInstrBuilder MIB;
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
  LoongArchFormalArgHandler Handler(MIRBuilder, MF.getRegInfo());

  return determineAndHandleAssignments(Handler, Assigner, SplitArgInfos,
                                       MIRBuilder, CC, F.isVarArg());
}

bool LoongArchCallLowering::lowerCall(MachineIRBuilder &MIRBuilder,
                                      CallLoweringInfo &Info) const {
  MachineFunction &MF = MIRBuilder.getMF();
  const DataLayout &DL = MF.getDataLayout();
  const Function &F = MF.getFunction();
  CallingConv::ID CC = F.getCallingConv();

  // TODO: Support vararg functions.
  if (Info.IsVarArg)
    return false;

  // TODO: Support all argument types.
  for (auto &AInfo : Info.OrigArgs) {
    if (AInfo.Ty->isIntOrPtrTy())
      continue;
    return false;
  }

  SmallVector<ArgInfo, 32> SplitArgInfos;
  SmallVector<ISD::OutputArg, 8> Outs;
  for (auto &AInfo : Info.OrigArgs) {
    // Handle any required unmerging of split value types from a given VReg into
    // physical registers. ArgInfo objects are constructed correspondingly and
    // appended to SplitArgInfos.
    splitToValueTypes(AInfo, SplitArgInfos, DL, CC);
  }

  // TODO: Support tail calls.
  Info.IsTailCall = false;

  // If the callee is a GlobalAddress or ExternalSymbol and cannot be assumed as
  // DSOLocal, then use MO_CALL_PLT. Otherwise use MO_CALL.
  if (Info.Callee.isGlobal()) {
    const GlobalValue *GV = Info.Callee.getGlobal();
    unsigned OpFlags = getTLI()->getTargetMachine().shouldAssumeDSOLocal(GV)
                           ? LoongArchII::MO_CALL
                           : LoongArchII::MO_CALL_PLT;

    Info.Callee.setTargetFlags(OpFlags);
  } else if (Info.Callee.isSymbol()) {
    unsigned OpFlags =
        getTLI()->getTargetMachine().shouldAssumeDSOLocal(nullptr)
            ? LoongArchII::MO_CALL
            : LoongArchII::MO_CALL_PLT;

    Info.Callee.setTargetFlags(OpFlags);
  }

  MachineInstrBuilder Call =
      MIRBuilder
          .buildInstrNoInsert(Info.Callee.isReg()
                                  ? LoongArch::PseudoCALLIndirect
                                  : LoongArch::PseudoCALL)
          .add(Info.Callee);

  LoongArchOutgoingValueAssigner ArgAssigner(LoongArch::CC_LoongArch,
                                             /*IsRet=*/false);
  LoongArchOutgoingValueHandler ArgHandler(MIRBuilder, MF.getRegInfo(), Call);
  if (!determineAndHandleAssignments(ArgHandler, ArgAssigner, SplitArgInfos,
                                     MIRBuilder, CC, Info.IsVarArg))
    return false;

  MIRBuilder.insertInstr(Call);

  // If Callee is a reg, since it is used by a target specific
  // instruction, it must have a register class matching the
  // constraint of that instruction.
  const LoongArchSubtarget &Subtarget =
      MIRBuilder.getMF().getSubtarget<LoongArchSubtarget>();
  const TargetRegisterInfo *TRI = Subtarget.getRegisterInfo();
  if (Call->getOperand(0).isReg())
    constrainOperandRegClass(MF, *TRI, MF.getRegInfo(),
                             *Subtarget.getInstrInfo(),
                             *Subtarget.getRegBankInfo(), *Call,
                             Call->getDesc(), Call->getOperand(0), 0);

  if (Info.OrigRet.Ty->isVoidTy())
    return true;

  // TODO: Only integer, pointer and aggregate types are supported now.
  if (!Info.OrigRet.Ty->isIntOrPtrTy() && !Info.OrigRet.Ty->isAggregateType())
    return false;

  SmallVector<ArgInfo, 4> SplitRetInfos;
  splitToValueTypes(Info.OrigRet, SplitRetInfos, DL, CC);

  // Assignments should be handled *before* the merging of values takes place.
  // To ensure this, the insert point is temporarily adjusted to just after the
  // call instruction.
  MachineBasicBlock::iterator CallInsertPt = Call;
  MIRBuilder.setInsertPt(MIRBuilder.getMBB(), std::next(CallInsertPt));

  LoongArchIncomingValueAssigner RetAssigner(LoongArch::CC_LoongArch,
                                             /*IsRet=*/true);
  LoongArchCallReturnHandler RetHandler(MIRBuilder, MF.getRegInfo(), Call);
  if (!determineAndHandleAssignments(RetHandler, RetAssigner, SplitRetInfos,
                                     MIRBuilder, CC, Info.IsVarArg))
    return false;

  // Readjust insert point to end of basic block.
  MIRBuilder.setMBB(MIRBuilder.getMBB());

  return true;
}
