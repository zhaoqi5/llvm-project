//===- bolt/Target/LoongArch/LoongArchMCPlusBuilder.cpp -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides LoongArch-specific MCPlus builder.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/LoongArchMCExpr.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "bolt/Core/MCPlusBuilder.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

#define DEBUG_TYPE "mcplus"

using namespace llvm;
using namespace bolt;

namespace {

class LoongArchMCPlusBuilder : public MCPlusBuilder {
public:
  using MCPlusBuilder::MCPlusBuilder;

  bool shouldRecordCodeRelocation(uint64_t RelType) const override {
    switch (RelType) {
    case ELF::R_LARCH_B26:
    case ELF::R_LARCH_PCALA_LO12:
    case ELF::R_LARCH_PCALA_HI20:
    case ELF::R_LARCH_GOT_PC_LO12:
    case ELF::R_LARCH_GOT_PC_HI20:
    case ELF::R_LARCH_GOT64_PC_LO20:
    case ELF::R_LARCH_GOT64_PC_HI12:
      return true;
    default:
      llvm_unreachable("Unexpected LoongArch relocation type in code");
    }
  }

  bool isIndirectCall(const MCInst &Inst) const override {
    if (!isCall(Inst))
      return false;

    switch (Inst.getOpcode()) {
    default:
      return false;
    case LoongArch::JIRL:
      return true;
    }
  }

  bool isNoop(const MCInst &Inst) const override {
    return Inst.getOpcode() == LoongArch::ANDI &&
           Inst.getOperand(0).getReg() == LoongArch::R0 &&
           Inst.getOperand(1).getReg() == LoongArch::R0 &&
           Inst.getOperand(2).getImm() == 0;
  }

  bool hasPCRelOperand(const MCInst &Inst) const override {
    switch (Inst.getOpcode()) {
    default:
      return false;
    case LoongArch::B:
    case LoongArch::BL:
      return true;
    }
  }

  bool replaceBranchTarget(MCInst &Inst, const MCSymbol *TBB,
                           MCContext *Ctx) const override {
    assert((isCall(Inst) || isBranch(Inst)) && !isIndirectBranch(Inst) &&
           "Invalid instruction");

    unsigned SymOpIndex;
    auto Result = getSymbolRefOperandNum(Inst, SymOpIndex);
    (void)Result;
    assert(Result && "unimplemented branch");

    Inst.getOperand(SymOpIndex) = MCOperand::createExpr(
        MCSymbolRefExpr::create(TBB, MCSymbolRefExpr::VK_None, *Ctx));
    return true;
  }

  IndirectBranchType analyzeIndirectBranch(
      MCInst &Instruction, InstructionIterator Begin, InstructionIterator End,
      const unsigned PtrSize, MCInst *&MemLocInstr, unsigned &BaseRegNum,
      unsigned &IndexRegNum, int64_t &DispValue, const MCExpr *&DispExpr,
      MCInst *&PCRelBaseOut) const override {
    MemLocInstr = nullptr;
    BaseRegNum = 0;
    IndexRegNum = 0;
    DispValue = 0;
    DispExpr = nullptr;
    PCRelBaseOut = nullptr;
    return IndirectBranchType::UNKNOWN;
  }

  bool convertJmpToTailCall(MCInst &Inst) override {
    if (isTailCall(Inst))
      return false;

    setTailCall(Inst);
    return true;
  }

  bool createReturn(MCInst &Inst) const override {
    Inst.setOpcode(LoongArch::JIRL);
    Inst.clear();
    Inst.addOperand(MCOperand::createReg(LoongArch::R0));
    Inst.addOperand(MCOperand::createReg(LoongArch::R1));
    Inst.addOperand(MCOperand::createImm(0));
    return true;
  }

  bool createUncondBranch(MCInst &Inst, const MCSymbol *TBB,
                          MCContext *Ctx) const override {
    Inst.setOpcode(LoongArch::B);
    Inst.clear();
    Inst.addOperand(MCOperand::createExpr(
        MCSymbolRefExpr::create(TBB, MCSymbolRefExpr::VK_None, *Ctx)));
    return true;
  }

  StringRef getTrapFillValue() const override {
    return StringRef("\0\0\0\0", 4);
  }

  bool createLoongArchCall(MCInst &InstA, MCInst &InstB, const MCSymbol *Target,
                           MCContext *Ctx, bool isTailCall) override {
    InstA.setOpcode(LoongArch::PCADDU18I);
    InstA.clear();
    if (isTailCall)
      InstA.addOperand(MCOperand::createReg(LoongArch::R20));
    else
      InstA.addOperand(MCOperand::createReg(LoongArch::R1));
    InstA.addOperand(MCOperand::createExpr(LoongArchMCExpr::create(
        MCSymbolRefExpr::create(Target, MCSymbolRefExpr::VK_None, *Ctx),
        LoongArchMCExpr::VK_LoongArch_CALL36, *Ctx)));

    InstB.setOpcode(LoongArch::JIRL);
    InstB.clear();
    if (isTailCall) {
      InstB.addOperand(MCOperand::createReg(LoongArch::R0));
      InstB.addOperand(MCOperand::createReg(LoongArch::R20));
    } else {
      InstB.addOperand(MCOperand::createReg(LoongArch::R1));
      InstB.addOperand(MCOperand::createReg(LoongArch::R1));
    }
    InstB.addOperand(MCOperand::createImm(0));

    return true;
  }

  bool analyzeBranch(InstructionIterator Begin, InstructionIterator End,
                     const MCSymbol *&TBB, const MCSymbol *&FBB,
                     MCInst *&CondBranch,
                     MCInst *&UncondBranch) const override {
    auto I = End;

    while (I != Begin) {
      --I;

      // Ignore nops and CFIs
      if (isPseudo(*I) || isNoop(*I))
        continue;

      // Stop when we find the first non-terminator
      if (!isTerminator(*I) || isTailCall(*I) || !isBranch(*I))
        break;

      // Handle unconditional branches.
      if (isUnconditionalBranch(*I)) {
        // If any code was seen after this unconditional branch, we've seen
        // unreachable code. Ignore them.
        CondBranch = nullptr;
        UncondBranch = &*I;
        const MCSymbol *Sym = getTargetSymbol(*I);
        assert(Sym != nullptr &&
               "Couldn't extract BB symbol from jump operand");
        TBB = Sym;
        continue;
      }

      // Handle conditional branches and ignore indirect branches
      if (isIndirectBranch(*I))
        return false;

      if (CondBranch == nullptr) {
        const MCSymbol *TargetBB = getTargetSymbol(*I);
        if (TargetBB == nullptr) {
          // Unrecognized branch target
          return false;
        }
        FBB = TBB;
        TBB = TargetBB;
        CondBranch = &*I;
        continue;
      }

      llvm_unreachable("multiple conditional branches in one BB");
    }
    return true;
  }

  bool getSymbolRefOperandNum(const MCInst &Inst, unsigned &OpNum) const {
    switch (Inst.getOpcode()) {
    default:
      return false;
    case LoongArch::B:
    case LoongArch::BL:
      OpNum = 0;
      return true;
    case LoongArch::BEQZ:
    case LoongArch::BNEZ:
    case LoongArch::BCEQZ:
    case LoongArch::BCNEZ:
      OpNum = 1;
      return true;
    case LoongArch::BEQ:
    case LoongArch::BNE:
    case LoongArch::BLT:
    case LoongArch::BGE:
    case LoongArch::BLTU:
    case LoongArch::BGEU:
    case LoongArch::JIRL:
      OpNum = 2;
      return true;
    }
  }

  const MCSymbol *getTargetSymbol(const MCExpr *Expr) const override {
    auto *LoongArchExpr = dyn_cast<LoongArchMCExpr>(Expr);
    if (LoongArchExpr && LoongArchExpr->getSubExpr())
      return getTargetSymbol(LoongArchExpr->getSubExpr());

    auto *BinExpr = dyn_cast<MCBinaryExpr>(Expr);
    if (BinExpr)
      return getTargetSymbol(BinExpr->getLHS());

    auto *SymExpr = dyn_cast<MCSymbolRefExpr>(Expr);
    if (SymExpr && SymExpr->getKind() == MCSymbolRefExpr::VK_None)
      return &SymExpr->getSymbol();

    return nullptr;
  }

  const MCSymbol *getTargetSymbol(const MCInst &Inst,
                                  unsigned OpNum = 0) const override {
    if (!getSymbolRefOperandNum(Inst, OpNum))
      return nullptr;

    const MCOperand &Op = Inst.getOperand(OpNum);
    if (!Op.isExpr())
      return nullptr;

    return getTargetSymbol(Op.getExpr());
  }

  ///  Matches PLT entry pattern and returns the associated GOT entry address.
  ///  Typical PLT entry looks like the following:
  ///
  ///    pcaddu12i    t3, 8(0x8)
  ///    ld.d         t3, t3, offset
  ///    jirl         t1, t3, 0
  ///    nop
  ///
  uint64_t analyzePLTEntry(MCInst &Instruction, InstructionIterator Begin,
                           InstructionIterator End,
                           uint64_t BeginPC) const override {
    auto I = Begin;

    assert(I != End);
    auto &PCADD = *I++;
    assert(PCADD.getOpcode() == LoongArch::PCADDU12I);
    assert(PCADD.getOperand(0).getReg() == LoongArch::R15);

    assert(I != End);
    auto &LD = *I++;
    assert(LD.getOpcode() == LoongArch::LD_D);
    assert(LD.getOperand(0).getReg() == LoongArch::R15);
    assert(LD.getOperand(1).getReg() == LoongArch::R15);

    assert(I != End);
    auto &JIRL = *I++;
    (void)JIRL;
    assert(JIRL.getOpcode() == LoongArch::JIRL);
    assert(JIRL.getOperand(0).getReg() == LoongArch::R13);
    assert(JIRL.getOperand(1).getReg() == LoongArch::R15);

    assert(I != End);
    auto &NOP = *I++;
    (void)NOP;
    assert(isNoop(NOP));

    assert(I == End);

    auto PCADDOffset = PCADD.getOperand(1).getImm() << 12;
    auto LDOffset = LD.getOperand(2).getImm();
    return BeginPC + PCADDOffset + LDOffset;
  }

  bool replaceImmWithSymbolRef(MCInst &Inst, const MCSymbol *Symbol,
                               int64_t Addend, MCContext *Ctx, int64_t &Value,
                               uint64_t RelType) const override {
    unsigned ImmOpNo = -1U;
    for (unsigned Index = 0; Index < MCPlus::getNumPrimeOperands(Inst);
         ++Index) {
      if (Inst.getOperand(Index).isImm()) {
        ImmOpNo = Index;
        break;
      }
    }
    if (ImmOpNo == -1U)
      return false;

    Value = Inst.getOperand(ImmOpNo).getImm();

    setOperandToSymbolRef(Inst, ImmOpNo, Symbol, Addend, Ctx, RelType);

    return true;
  }

  const MCExpr *getTargetExprFor(MCInst &Inst, const MCExpr *Expr,
                                 MCContext &Ctx,
                                 uint64_t RelType) const override {
    switch (RelType) {
    default:
      return Expr;
    case ELF::R_LARCH_B26:
      return LoongArchMCExpr::create(Expr, LoongArchMCExpr::VK_LoongArch_B26,
                                     Ctx);
    case ELF::R_LARCH_PCALA_LO12:
      return LoongArchMCExpr::create(
          Expr, LoongArchMCExpr::VK_LoongArch_PCALA_LO12, Ctx);
    case ELF::R_LARCH_PCALA_HI20:
      return LoongArchMCExpr::create(
          Expr, LoongArchMCExpr::VK_LoongArch_PCALA_HI20, Ctx);
    case ELF::R_LARCH_GOT_PC_LO12:
      return LoongArchMCExpr::create(
          Expr, LoongArchMCExpr::VK_LoongArch_GOT_PC_LO12, Ctx);
    case ELF::R_LARCH_GOT_PC_HI20:
      return LoongArchMCExpr::create(
          Expr, LoongArchMCExpr::VK_LoongArch_GOT_PC_HI20, Ctx);
    case ELF::R_LARCH_GOT64_PC_LO20:
      return LoongArchMCExpr::create(
          Expr, LoongArchMCExpr::VK_LoongArch_GOT64_PC_LO20, Ctx);
    case ELF::R_LARCH_GOT64_PC_HI12:
      return LoongArchMCExpr::create(
          Expr, LoongArchMCExpr::VK_LoongArch_GOT64_PC_HI12, Ctx);
    }
  }

  unsigned getInvertedBranchOpcode(unsigned Opcode) const {
    switch (Opcode) {
    default:
      llvm_unreachable("Failed to invert branch opcode");
      return Opcode;
    case LoongArch::BEQ:
      return LoongArch::BNE;
    case LoongArch::BNE:
      return LoongArch::BEQ;
    case LoongArch::BEQZ:
      return LoongArch::BNEZ;
    case LoongArch::BNEZ:
      return LoongArch::BEQZ;
    case LoongArch::BCEQZ:
      return LoongArch::BCNEZ;
    case LoongArch::BCNEZ:
      return LoongArch::BCEQZ;
    case LoongArch::BLT:
      return LoongArch::BGE;
    case LoongArch::BGE:
      return LoongArch::BLT;
    case LoongArch::BLTU:
      return LoongArch::BGEU;
    case LoongArch::BGEU:
      return LoongArch::BLTU;
    }
  }

  bool reverseBranchCondition(MCInst &Inst, const MCSymbol *TBB,
                              MCContext *Ctx) const override {
    Inst.setOpcode(getInvertedBranchOpcode(Inst.getOpcode()));
    return replaceBranchTarget(Inst, TBB, Ctx);
  }

  bool lowerTailCall(MCInst &Inst) override {
    removeAnnotation(Inst, MCPlus::MCAnnotation::kTailCall);
    if (getConditionalTailCall(Inst))
      unsetConditionalTailCall(Inst);
    return true;
  }

  uint16_t getMinFunctionAlignment() const override { return 4; }

  void getCalleeSavedRegs(BitVector &Regs) const override {
    Regs |= getAliases(LoongArch::R22);
    Regs |= getAliases(LoongArch::R23);
    Regs |= getAliases(LoongArch::R24);
    Regs |= getAliases(LoongArch::R25);
    Regs |= getAliases(LoongArch::R26);
    Regs |= getAliases(LoongArch::R27);
    Regs |= getAliases(LoongArch::R28);
    Regs |= getAliases(LoongArch::R29);
    Regs |= getAliases(LoongArch::R30);
    Regs |= getAliases(LoongArch::R31);
  }
};

} // end anonymous namespace

namespace llvm {
namespace bolt {

MCPlusBuilder *createLoongArchMCPlusBuilder(const MCInstrAnalysis *Analysis,
                                            const MCInstrInfo *Info,
                                            const MCRegisterInfo *RegInfo,
                                            const MCSubtargetInfo *STI) {
  return new LoongArchMCPlusBuilder(Analysis, Info, RegInfo, STI);
}

} // namespace bolt
} // namespace llvm
