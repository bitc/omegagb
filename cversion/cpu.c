#include <stdio.h>

#include "cpu.h"
#include "memory.h"
#include "machine.h"

inline static void NOP(Cpu* cpu, Memory* memory)
{
	cpu->pc += 1;
}

inline static void LD2(Cpu* cpu, Memory* memory, enum RegisterPair rp)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 np = read_memory(memory, cpu->pc + 2);
	u16 nn = join_word_16(np, n);

	write_register_pair(cpu, rp, nn);
	cpu->pc += 3;
}

static inline void LDPR(Cpu* cpu, Memory* memory, enum RegisterPair rp)
{
	u16 a = read_register_pair(cpu, rp);
	u8 v = cpu->a;
	write_memory(memory, a, v);
	cpu->pc += 1;
}

static inline void INC2(Cpu* cpu, Memory* memory, enum RegisterPair rp)
{
	u16 v = read_register_pair(cpu, rp);
	u16 vp = v + 1;
	write_register_pair(cpu, rp, vp);
	cpu->pc += 1;
}

/* PERFECT (the correct way for updating half-carry flag) */
static inline void INC(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = read_register(cpu, r);
	u8 vp = v + 1;
	write_register(cpu, r, vp);
	write_flags(cpu, vp == 0, 0, (v & 0x0F) > 0, -1);
	cpu->pc += 1;
}

static inline void DEC(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = read_register(cpu, r);
	u8 vp = v - 1;
	write_register(cpu, r, vp);
	write_flags(cpu, vp == 0, 1, v != 0xF0, -1);
	cpu->pc += 1;
}

static inline void LDRN(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	write_register(cpu, r, n);
	cpu->pc += 2;
}

static inline void RLCA(Cpu* cpu, Memory* memory)
{
	u8 v = cpu->a;
	u8 vp = (v << 1) | ((v >> 7) & 1);
	cpu->a = vp;
	write_flags(cpu, vp == 0, 0, 0, (v & (1 << 7)) > 0);
	cpu->pc += 1;
}

static inline void LDP2(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 np = read_memory(memory, cpu->pc + 2);
	u16 nn = join_word_16(np, n);

	u8 hi, lo;
	split_word_16(cpu->sp, &hi, &lo);
	write_memory(memory, nn, hi);
	write_memory(memory, nn + 1, lo);
	cpu->pc += 3;
}

static inline void ADD2HL(Cpu* cpu, Memory* memory, enum RegisterPair rp)
{
	u16 v = read_register_pair(cpu, HL);
	u16 vp = read_register_pair(cpu, rp);
	u16 vpp = v + vp;
	write_register_pair(cpu, HL, vpp);
	write_flags(cpu, -1, -1, -1, vpp < v);
	cpu->pc += 1;
}

static inline void LDAP(Cpu* cpu, Memory* memory, enum RegisterPair rp)
{
	u16 a = read_register_pair(cpu, rp);
	u8 v = read_memory(memory, a);
	cpu->a = v;
	cpu->pc += 1;
}

static inline void DEC2(Cpu* cpu, Memory* memory, enum RegisterPair rp)
{
	u16 v = read_register_pair(cpu, rp);
	u16 vp = v - 1;
	write_register_pair(cpu, rp, vp);
	cpu->pc += 1;
}

static inline void RRCA(Cpu* cpu, Memory* memory)
{
	u8 v = cpu->a;
	u8 vp = ((v >> 1) & ~(1 << 7)) | (v << 7);
	cpu->a = vp;
	write_flags(cpu, vp == 0, 0, 0, (v & (1 << 0)) > 0);
	cpu->pc += 1;
}

static inline void STOP(Cpu* cpu, Memory* memory)
{
	assert(0);
	cpu->pc += 2;
}

static inline void RLA(Cpu* cpu, Memory* memory)
{
	u8 v = cpu->a;
	u8 c = cpu->f & (1 << 4);
	u8 vp = c ? (v << 1) | (1 << 0) : v << 1;
	cpu->a = vp;
	write_flags(cpu, vp == 0, 0, 0, (v & (1 << 7)) > 0);
	cpu->pc += 1;
}

static inline int test_flag_condition(Cpu* cpu, enum FlagCondition fc)
{
	switch(fc)
	{
		case FLAG_NONE:
			return 1;
		case FLAG_NZ:
			return !(cpu->f & (1 << 7));
		case FLAG_Z:
			return cpu->f & (1 << 7);
		case FLAG_NC:
			return !(cpu->f & (1 << 4));
		case FLAG_C:
			return cpu->f & (1 << 4);
	};
	assert(0);
}

static inline void JR(Cpu* cpu, Memory* memory, enum FlagCondition fc)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	s8 d = (s8)(n);

	if(test_flag_condition(cpu, fc))
		cpu->pc += d + 2;
	else
		cpu->pc += 2;
}

static inline void RRA(Cpu* cpu, Memory* memory)
{
	assert(0);
}

static inline void LDIM(Cpu* cpu, Memory* memory)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = cpu->a;
	write_memory(memory, a, v);
	u16 ap = a + 1;
	write_register_pair(cpu, HL, ap);
	cpu->pc += 1;
}

static inline void DAA(Cpu* cpu, Memory* memory)
{
	assert(0);
}

static inline void LDIR(Cpu* cpu, Memory* memory)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = read_memory(memory, a);
	cpu->a = v;
	u16 ap = a + 1;
	write_register_pair(cpu, HL, ap);
	cpu->pc += 1;
}

static inline void CPL(Cpu* cpu, Memory* memory)
{
	u8 v = cpu->a;
	cpu->a = ~v;
	write_flags(cpu, -1, 1, 1, -1);
	cpu->pc += 1;
}

static inline void LDDM(Cpu* cpu, Memory* memory)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = cpu->a;
	write_memory(memory, a, v);
	write_register_pair(cpu, HL, a - 1);
	cpu->pc += 1;
}

static inline void INCHL(Cpu* cpu, Memory* memory)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = read_memory(memory, a);
	u8 vp = v + 1;
	write_memory(memory, a, vp);
	write_flags(cpu, vp == 0, 0, v == 0x0F, -1);
	cpu->pc += 1;
}

static inline void DECHL(Cpu* cpu, Memory* memory)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = read_memory(memory, a);
	write_memory(memory, a, v - 1);
	write_flags(cpu, (v - 1) == 0, 1, v != 0xF0, -1);
	cpu->pc += 1;
}

static inline void LDHLN(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u16 a = read_register_pair(cpu, HL);
	write_memory(memory, a, n);
	cpu->pc += 2;
}

static inline void SCF(Cpu* cpu, Memory* memory)
{
	write_flags(cpu, -1, 0, 0, 1);
	cpu->pc += 1;
}

static inline void LDDR(Cpu* cpu, Memory* memory)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = read_memory(memory, a);
	cpu->a = v;
	write_register_pair(cpu, HL, a - 1);
	cpu->pc += 1;
}

static inline void CCF(Cpu* cpu, Memory* memory)
{
	u8 c = cpu->f & (1 << 4);
	write_flags(cpu, -1, 0, 0, !c);
	cpu->pc += 1;
}

static inline void LDR(Cpu* cpu, Memory* memory, enum Register r1, enum Register r2)
{
	u8 v = read_register(cpu, r2);
	write_register(cpu, r1, v);
	cpu->pc += 1;
}

static inline void LDRHL(Cpu* cpu, Memory* memory, enum Register r)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = read_memory(memory, a);
	write_register(cpu, r, v);
	cpu->pc += 1;
}

static inline void HALT(Cpu* cpu, Memory* memory)
{
	cpu->pc += 2;
}

static inline void LDHL(Cpu* cpu, Memory* memory, enum Register r)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = read_register(cpu, r);
	write_memory(memory, a, v);
	cpu->pc += 1;
}

static inline void ADD(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = cpu->a;
	u8 vp = read_register(cpu, r);
	u8 vpp = v + vp;
	cpu->a = vpp;
	write_flags(cpu, vpp == 0, 0, (vpp & 0x0F) < (v & 0x0F), vpp < v);
	cpu->pc += 1;
}

static inline void ADDHL(Cpu* cpu, Memory* memory)
{
	u8 v = cpu->a;
	u16 a = read_register_pair(cpu, HL);
	u8 vp = read_memory(memory, a);
	u8 vpp = v + vp;
	cpu->a = vpp;
	write_flags(cpu, vpp == 0, 0, (vpp & 0x0F) < (v & 0x0F), vpp < v);
	cpu->pc += 1;
}

static inline void ADC(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = cpu->a;
	u8 vp = read_register(cpu, r);
	u8 carry = cpu->f & (1 << 4) ? 1 : 0;
	u8 vpp = v + vp + carry;
	cpu->a = vpp;
	write_flags(cpu, vpp == 0, 0, (vpp & 0x0F) < (v & 0x0F), vpp < v);
	cpu->pc += 1;
}

static inline void ADCHL(Cpu* cpu, Memory* memory)
{
	assert(0);
	u8 v = cpu->a;
	u16 a = read_register_pair(cpu, HL);
	u8 vp = read_memory(memory, a);
	u8 carry = cpu->f & (1 << 4) ? 1 : 0;
	u8 vpp = v + vp + carry;
	cpu->a = vpp;
	write_flags(cpu, vpp == 0, 0, (vpp & 0x0F) < (v & 0x0F), vpp < v);
	cpu->pc += 1;
}

static inline void SUB(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = cpu->a;
	u8 vp = read_register(cpu, r);
	u8 vpp = v - vp;
	cpu->a = vpp;
	write_flags(cpu, vpp == 0, 1, (vpp & 0x0F) > (v & 0x0F), vpp > v);
	cpu->pc += 1;
}

static inline void SUBHL(Cpu* cpu, Memory* memory)
{
	u8 v = cpu->a;
	u16 a = read_register_pair(cpu, HL);
	u8 vp = read_memory(memory, a);
	u8 vpp = v - vp;
	cpu->a = vpp;
	write_flags(cpu, vpp == 0, 1, (vpp & 0x0F) > (v & 0x0F), vpp > v);
	cpu->pc += 1;
}

static inline void SBC(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = cpu->a;
	u8 vp = read_register(cpu, r);
	u8 carry = cpu->f & (1 << 4) ? 1 : 0;
	u8 vpp = v - vp - carry;
	cpu->a = vpp;
	write_flags(cpu, vpp == 0, 1, (vpp & 0x0F) > (v & 0x0F), vpp > v);
	cpu->pc += 1;
}

static inline void SBCHL(Cpu* cpu, Memory* memory)
{
	assert(0);
	u8 v = cpu->a;
	u16 a = read_register_pair(cpu, HL);
	u8 vp = read_memory(memory, a);
	u8 carry = cpu->f & (1 << 4) ? 1 : 0;
	u8 vpp = v - vp - carry;
	cpu->a = vpp;
	write_flags(cpu, vpp == 0, 1, (vpp & 0x0F) > (v & 0x0F), vpp > v);
	cpu->pc += 1;
}

static inline void AND(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = cpu->a;
	u8 vp = read_register(cpu, r);
	u8 vpp = v & vp;
	cpu->a = vpp;
	write_flags(cpu, vpp == 0, 0, 1, 0);
	cpu->pc += 1;
}

static inline void ANDHL(Cpu* cpu, Memory* memory)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = cpu->a;
	u8 vp = read_memory(memory, a);
	u8 vpp = v & vp;
	cpu->a = vpp;
	write_flags(cpu, vpp == 0, 0, 1, 0);
	cpu->pc += 1;
}

static inline void XOR(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = cpu->a;
	u8 vp = read_register(cpu, r);
	u8 vpp = v ^ vp;
	cpu->a = vpp;
	write_flags(cpu, vpp == 0, 0, 0, 0);
	cpu->pc += 1;
}

static inline void XORHL(Cpu* cpu, Memory* memory)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = cpu->a;
	u8 vp = read_memory(memory, a);
	u8 vpp = v ^ vp;
	cpu->a = vpp;
	write_flags(cpu, vpp == 0, 0, 0, 0);
	cpu->pc += 1;
}

static inline void OR(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = cpu->a;
	u8 vp = read_register(cpu, r);
	u8 vpp = v | vp;
	cpu->a = vpp;
	write_flags(cpu, vpp == 0, 0, 0, 0);
	cpu->pc += 1;

}

static inline void ORHL(Cpu* cpu, Memory* memory)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = cpu->a;
	u8 vp = read_memory(memory, a);
	u8 vpp = v | vp;
	cpu->a = vpp;
	write_flags(cpu, vpp == 0, 0, 0, 0);
	cpu->pc += 1;
}

static inline void CP(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = cpu->a;
	u8 vp = read_register(cpu, r);
	write_flags(cpu, v == vp, 1, (v & 0x0F) < (vp & 0x0F), v < vp);
	cpu->pc += 1;
}

static inline void CPHL(Cpu* cpu, Memory* memory)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = cpu->a;
	u8 vp = read_memory(memory, a);
	write_flags(cpu, v == vp, 1, (v & 0x0F) < (vp & 0x0F), v < vp);
	cpu->pc += 1;
}

static inline void RET(Cpu* cpu, Memory* memory, enum FlagCondition fc)
{
	if(test_flag_condition(cpu, fc))
	{
		u16 a = read_register_pair(cpu, SP);
		u8 v = read_memory(memory, a);
		u8 vp = read_memory(memory, a + 1);
		u16 vpp = join_word_16(vp, v);
		cpu->pc = vpp;
		cpu->sp = a + 2;
	}
	else
	{
		cpu->pc += 1;
	}
}

static inline void POP(Cpu* cpu, Memory* memory, enum StackRegister sr)
{
	u16 a = cpu->sp;
	u8 lo = read_memory(memory, a);
	u8 hi = read_memory(memory, a + 1);
	u16 v = join_word_16(hi, lo);
	write_stack_register_pair(cpu, sr, v);
	cpu->sp = a + 2;
	cpu->pc += 1;
}

static inline void JP(Cpu* cpu, Memory* memory, enum FlagCondition fc)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 np = read_memory(memory, cpu->pc + 2);
	u16 nn = join_word_16(np, n);

	if(test_flag_condition(cpu, fc))
		cpu->pc = nn;
	else
		cpu->pc += 3;
}

static inline void CALL(Cpu* cpu, Memory* memory, enum FlagCondition fc)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 np = read_memory(memory, cpu->pc + 2);
	u16 nn = join_word_16(np, n);

	if(test_flag_condition(cpu, fc))
	{
		u16 pc = cpu->pc;
		u16 a = cpu->sp;
		u8 hi, lo;
		split_word_16(pc + 3, &hi, &lo);
		write_memory(memory, a - 1, hi);
		write_memory(memory, a - 2, lo);
		cpu->sp = a - 2;
		cpu->pc = nn;
	}
	else
	{
		cpu->pc += 3;
	}
}

static inline void PUSH(Cpu* cpu, Memory* memory, enum StackRegister sr)
{
	u16 a = cpu->sp;
	u16 v = read_stack_register_pair(cpu, sr);
	u8 hi, lo;
	split_word_16(v, &hi, &lo);
	write_memory(memory, a - 1, hi);
	write_memory(memory, a - 2, lo);
	cpu->sp = a - 2;
	cpu->pc += 1;
}

static inline void ADDN(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 v = cpu->a;
	u8 vp = v + n;
	cpu->a = vp;
	write_flags(cpu, vp == 0, 0, (vp & 0x0F) < (v & 0x0F), vp < v);
	cpu->pc += 2;
}

static inline void RST(Cpu* cpu, Memory* memory, u16 ja)
{
	u8 v, vp;
	split_word_16(cpu->pc + 1, &v, &vp);
	u16 a = cpu->sp;
	write_memory(memory, a - 1, v);
	write_memory(memory, a - 2, vp);
	cpu->sp = a - 2;
	cpu->pc = ja;
}

static inline void ADCN(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 v = cpu->a;
	u8 carry = cpu->f & (1 << 4) ? 1 : 0;
	u8 vp = v + n + carry;
	cpu->a = vp;
	write_flags(cpu, vp == 0, 0, (vp & 0x0F) < (v & 0x0F), vp < v);
	cpu->pc += 2;
}

static inline void SUBN(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 v = cpu->a;
	u8 vp = v - n;
	cpu->a = vp;
	write_flags(cpu, vp == 0, 1, (vp & 0x0F) > (v & 0x0F), vp > v);
	cpu->pc += 2;
}

static inline void RETI(Cpu* cpu, Memory* memory)
{
	RET(cpu, memory, FLAG_NONE);
}

static inline void SBCN(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 v = cpu->a;
	u8 carry = cpu->f & (1 << 4) ? 1 : 0;
	u8 vp = v - n - carry;
	cpu->a = vp;
	write_flags(cpu, vp == 0, 1, (vp & 0x0F) > (v & 0x0F), vp > v);
	cpu->pc += 2;
}

static inline void LDHM(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u16 a = 0xFF00 + (u16)(n);
	u8 v = cpu->a;
	write_memory(memory, a, v);
	cpu->pc += 2;
}

static inline void LDHR(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u16 a = 0xFF00 + (u16)(n);
	u8 v = read_memory(memory, a);
	cpu->a = v;
	cpu->pc += 2;
}

static inline void LDPC(Cpu* cpu, Memory* memory)
{
	u8 o = cpu->c;
	u16 a = 0xFF00 + (u16)(o);
	u8 v = cpu->a;
	write_memory(memory, a, v);
	cpu->pc += 1;
}

static inline void ANDN(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 v = cpu->a;
	u8 vp = v & n;
	cpu->a = vp;
	write_flags(cpu, vp == 0, 0, 1, 0);
	cpu->pc += 2;
}

static inline void ADD2SP(Cpu* cpu, Memory* memory)
{
	/* u8 n = read_memory(memory, cpu->pc + 1); */
	/* u8 s = (s8)(n); */

	fprintf(stderr, "[NOT IMPLEMENTED] ADD SP,o\n");
	assert(0);
}

static inline void JPHL(Cpu* cpu, Memory* memory)
{
	u16 a = read_register_pair(cpu, HL);
	cpu->pc = a;
}

static inline void LDPN(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 np = read_memory(memory, cpu->pc + 2);
	u16 nn = join_word_16(np, n);

	u8 v = cpu->a;
	write_memory(memory, nn, v);
	cpu->pc += 3;
}

static inline void XORN(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 v = cpu->a;
	u8 vp = v ^ n;
	cpu->a = vp;
	write_flags(cpu, vp == 0, 0, 0, 0);
	cpu->pc += 2;
}

static inline void LDAC(Cpu* cpu, Memory* memory)
{
	u8 o = cpu->c;
	u16 a = 0xFF00 + (u16)(o);
	u8 v = read_memory(memory, a);
	cpu->a = v;
	cpu->pc += 1;
}

static inline void DI(Cpu* cpu, Memory* memory)
{
	cpu->pc += 1;
}

static inline void ORN(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 v = cpu->a;
	u8 vp = v | n;
	cpu->a = vp;
	write_flags(cpu, vp == 0, 0, 0, 0);
	cpu->pc += 2;
}

static inline void LDHL2(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	s8 d = (s8)(n);

	u16 v = cpu->sp;
	u16 vp = v + d;
	write_register_pair(cpu, HL, vp);
	write_flags(cpu, 0, 0, (vp & 0x000F) < (v & 0x000F), vp < v);
	cpu->pc += 2;
}

static inline void LDSP2(Cpu* cpu, Memory* memory)
{
	u16 v = read_register_pair(cpu, HL);
	cpu->sp = v;
	cpu->pc += 1;
}

static inline void LDAPN(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 np = read_memory(memory, cpu->pc + 2);
	u16 nn = join_word_16(np, n);

	u8 v = read_memory(memory, nn);
	cpu->a = v;
	cpu->pc += 3;
}

static inline void EI(Cpu* cpu, Memory* memory)
{
	cpu->pc += 1;
}

static inline void CPN(Cpu* cpu, Memory* memory)
{
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 v = cpu->a;
	write_flags(cpu, v == n, 1, (v & 0x0F) < (n & 0x0F), v < n);
	cpu->pc += 2;
}

static inline void CBRLC(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = read_register(cpu, r);
	u8 vp = (v << 1) | ((v >> 7) & 1);
	write_register(cpu, r, vp);
	write_flags(cpu, vp == 0, 0, 0, (v & (1 << 7)) > 0);
}

static inline void CBRLCHL(Cpu* cpu, Memory* memory)
{
	assert(0);
	u16 a = read_register_pair(cpu, HL);
	u8 v = read_memory(memory, a);
	u8 vp = (v << 1) | ((v >> 7) & 1);
	write_memory(memory, a, vp);
	write_flags(cpu, vp == 0, 0, 0, v & (1 << 7));
}

static inline void CBRRC(Cpu* cpu, Memory* memory, enum Register r)
{
	assert(0);
	u8 v = read_register(cpu, r);
	u8 vp = ((v >> 1) & (~(1 << 7))) | (v << 7);
	write_register(cpu, r, vp);
	write_flags(cpu, vp == 0, 0, 0, v & 1);
}

static inline void CBRRCHL(Cpu* cpu, Memory* memory)
{
	assert(0);
	u16 a = read_register_pair(cpu, HL);
	u8 v = read_memory(memory, a);
	u8 vp = ((v >> 1) & (~(1 << 7))) | (v << 7);
	write_memory(memory, a, vp);
	write_flags(cpu, vp == 0, 0, 0, v & 1);
}

static inline void CBRL(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = read_register(cpu, r);
	u8 c = cpu->f & (1 << 4);
	u8 vp = c ? (v << 1) | (1 << 0) : v << 1;
	write_register(cpu, r, vp);
	write_flags(cpu, vp == 0, 0, 0, (v & (1 << 7)) > 0);
}

static inline void CBRLHL(Cpu* cpu, Memory* memory)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = read_memory(memory, a);
	u8 f = cpu->f;
	u8 c = f & (1 << 4);
	u8 vp = c ? (v << 1) | (1 << 0) : v << 1;
	write_memory(memory, a, vp);
	write_flags(cpu, vp == 0, 0, 0, (v & (1 << 7)) > 0);
}

static inline void CBRR(Cpu* cpu, Memory* memory, enum Register r)
{
	fprintf(stderr, "[NOT IMPLEMENTED] RR r\n");
	assert(0);
}

static inline void CBRRHL(Cpu* cpu, Memory* memory)
{
	fprintf(stderr, "[NOT IMPLEMENTED] RRHL\n");
	assert(0);
}

static inline void CBSLA(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = read_register(cpu, r);
	u8 vp = v << 1;
	write_register(cpu, r, vp);
	write_flags(cpu, vp == 0, 0, 0, (v & (1 << 7)) > 0);
}

static inline void CBSLAHL(Cpu* cpu, Memory* memory)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = read_memory(memory, a);
	u8 vp = v << 1;
	write_memory(memory, a, vp);
	write_flags(cpu, vp == 0, 0, 0, (v & (1 << 7)) > 0);
}

static inline void CBSRA(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = read_register(cpu, r);
	u8 vp = ((v >> 1) & 0x7F) | (v & 0x80);
	write_register(cpu, r, vp);
	write_flags(cpu, vp == 0, 0, 0, v & 0x01);
}

static inline void CBSRAHL(Cpu* cpu, Memory* memory)
{
	fprintf(stderr, "[NOT IMPLEMENTED] SRAHL\n");
	assert(0);
}

static inline void CBSWAP(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = read_register(cpu, r);
	u8 vp = (v << 4) | ((v >> 4) & 0x0F);
	write_register(cpu, r, vp);
	write_flags(cpu, vp == 0, 0, 0, 0);
}

static inline void CBSWAPHL(Cpu* cpu, Memory* memory)
{
	fprintf(stderr, "[NOT IMPLEMENTED] SWAPHL\n");
	assert(0);
}

static inline void CBSRL(Cpu* cpu, Memory* memory, enum Register r)
{
	u8 v = read_register(cpu, r);
	u8 vp = (v >> 1) & ~(1 << 7);
	write_register(cpu, r, vp);
	write_flags(cpu, vp == 0, 0, 0, (v & (1 << 0)) > 0);
}

static inline void CBSRLHL(Cpu* cpu, Memory* memory)
{
	fprintf(stderr, "[NOT IMPLEMENTED] SRLHL\n");
	assert(0);
}

static inline void CBBIT(Cpu* cpu, Memory* memory, int b, enum Register r)
{
	u8 v = read_register(cpu, r);
	u8 z = (v & (1 << b)) == 0;
	write_flags(cpu, z, 0, 1, -1);
}

static inline void CBBITHL(Cpu* cpu, Memory* memory, int b)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = read_memory(memory, a);
	u8 z = !(v & (1 << b));
	write_flags(cpu, z, 0, 1, -1);
}

static inline void CBRES(Cpu* cpu, Memory* memory, int b, enum Register r)
{
	u8 v = read_register(cpu, r);
	u8 vp = v & ~(1 << b);
	write_register(cpu, r, vp);
}

static inline void CBRESHL(Cpu* cpu, Memory* memory, int b)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = read_memory(memory, a);
	u8 vp = v & ~(1 << b);
	write_memory(memory, a, vp);
}

static inline void CBSET(Cpu* cpu, Memory* memory, int b, enum Register r)
{
	u8 v = read_register(cpu, r);
	u8 vp = v | (1 << b);
	write_register(cpu, r, vp);
}

static inline void CBSETHL(Cpu* cpu, Memory* memory, int b)
{
	u16 a = read_register_pair(cpu, HL);
	u8 v = read_memory(memory, a);
	u8 vp = v | (1 << b);
	write_memory(memory, a, vp);
}

static inline int CB(Cpu* cpu, Memory* memory)
{
	int cc; /* cycle count */

	u8 i = read_memory(memory, cpu->pc + 1);

	switch(i)
	{
		case 0x00: cc =  8; CBRLC    (cpu, memory, B);    break;
		case 0x01: cc =  8; CBRLC    (cpu, memory, C);    break;
		case 0x02: cc =  8; CBRLC    (cpu, memory, D);    break;
		case 0x03: cc =  8; CBRLC    (cpu, memory, E);    break;
		case 0x04: cc =  8; CBRLC    (cpu, memory, H);    break;
		case 0x05: cc =  8; CBRLC    (cpu, memory, L);    break;
		case 0x06: cc = 16; CBRLCHL  (cpu, memory);       break;
		case 0x07: cc =  8; CBRLC    (cpu, memory, A);    break;
		case 0x08: cc =  8; CBRRC    (cpu, memory, B);    break;
		case 0x09: cc =  8; CBRRC    (cpu, memory, C);    break;
		case 0x0A: cc =  8; CBRRC    (cpu, memory, D);    break;
		case 0x0B: cc =  8; CBRRC    (cpu, memory, E);    break;
		case 0x0C: cc =  8; CBRRC    (cpu, memory, H);    break;
		case 0x0D: cc =  8; CBRRC    (cpu, memory, L);    break;
		case 0x0E: cc = 16; CBRRCHL  (cpu, memory);       break;
		case 0x0F: cc =  8; CBRRC    (cpu, memory, A);    break;

		case 0x10: cc =  8; CBRL     (cpu, memory, B);    break;
		case 0x11: cc =  8; CBRL     (cpu, memory, C);    break;
		case 0x12: cc =  8; CBRL     (cpu, memory, D);    break;
		case 0x13: cc =  8; CBRL     (cpu, memory, E);    break;
		case 0x14: cc =  8; CBRL     (cpu, memory, H);    break;
		case 0x15: cc =  8; CBRL     (cpu, memory, L);    break;
		case 0x16: cc = 16; CBRLHL   (cpu, memory);       break;
		case 0x17: cc =  8; CBRL     (cpu, memory, A);    break;
		case 0x18: cc =  8; CBRR     (cpu, memory, B);    break;
		case 0x19: cc =  8; CBRR     (cpu, memory, C);    break;
		case 0x1A: cc =  8; CBRR     (cpu, memory, D);    break;
		case 0x1B: cc =  8; CBRR     (cpu, memory, E);    break;
		case 0x1C: cc =  8; CBRR     (cpu, memory, H);    break;
		case 0x1D: cc =  8; CBRR     (cpu, memory, L);    break;
		case 0x1E: cc = 16; CBRRHL   (cpu, memory);       break;
		case 0x1F: cc =  8; CBRR     (cpu, memory, A);    break;

		case 0x20: cc =  8; CBSLA    (cpu, memory, B);    break;
		case 0x21: cc =  8; CBSLA    (cpu, memory, C);    break;
		case 0x22: cc =  8; CBSLA    (cpu, memory, D);    break;
		case 0x23: cc =  8; CBSLA    (cpu, memory, E);    break;
		case 0x24: cc =  8; CBSLA    (cpu, memory, H);    break;
		case 0x25: cc =  8; CBSLA    (cpu, memory, L);    break;
		case 0x26: cc = 16; CBSLAHL  (cpu, memory);       break;
		case 0x27: cc =  8; CBSLA    (cpu, memory, A);    break;
		case 0x28: cc =  8; CBSRA    (cpu, memory, B);    break;
		case 0x29: cc =  8; CBSRA    (cpu, memory, C);    break;
		case 0x2A: cc =  8; CBSRA    (cpu, memory, D);    break;
		case 0x2B: cc =  8; CBSRA    (cpu, memory, E);    break;
		case 0x2C: cc =  8; CBSRA    (cpu, memory, H);    break;
		case 0x2D: cc =  8; CBSRA    (cpu, memory, L);    break;
		case 0x2E: cc = 16; CBSRAHL  (cpu, memory);       break;
		case 0x2F: cc =  8; CBSRA    (cpu, memory, A);    break;

		case 0x30: cc =  8; CBSWAP   (cpu, memory, B);    break;
		case 0x31: cc =  8; CBSWAP   (cpu, memory, C);    break;
		case 0x32: cc =  8; CBSWAP   (cpu, memory, D);    break;
		case 0x33: cc =  8; CBSWAP   (cpu, memory, E);    break;
		case 0x34: cc =  8; CBSWAP   (cpu, memory, H);    break;
		case 0x35: cc =  8; CBSWAP   (cpu, memory, L);    break;
		case 0x36: cc = 16; CBSWAPHL (cpu, memory);       break;
		case 0x37: cc =  8; CBSWAP   (cpu, memory, A);    break;
		case 0x38: cc =  8; CBSRL    (cpu, memory, B);    break;
		case 0x39: cc =  8; CBSRL    (cpu, memory, C);    break;
		case 0x3A: cc =  8; CBSRL    (cpu, memory, D);    break;
		case 0x3B: cc =  8; CBSRL    (cpu, memory, E);    break;
		case 0x3C: cc =  8; CBSRL    (cpu, memory, H);    break;
		case 0x3D: cc =  8; CBSRL    (cpu, memory, L);    break;
		case 0x3E: cc = 16; CBSRLHL  (cpu, memory);       break;
		case 0x3F: cc =  8; CBSRL    (cpu, memory, A);    break;

		case 0x40: cc =  8; CBBIT    (cpu, memory, 0, B); break;
		case 0x41: cc =  8; CBBIT    (cpu, memory, 0, C); break;
		case 0x42: cc =  8; CBBIT    (cpu, memory, 0, D); break;
		case 0x43: cc =  8; CBBIT    (cpu, memory, 0, E); break;
		case 0x44: cc =  8; CBBIT    (cpu, memory, 0, H); break;
		case 0x45: cc =  8; CBBIT    (cpu, memory, 0, L); break;
		case 0x46: cc = 12; CBBITHL  (cpu, memory, 0);    break;
		case 0x47: cc =  8; CBBIT    (cpu, memory, 0, A); break;
		case 0x48: cc =  8; CBBIT    (cpu, memory, 1, B); break;
		case 0x49: cc =  8; CBBIT    (cpu, memory, 1, C); break;
		case 0x4A: cc =  8; CBBIT    (cpu, memory, 1, D); break;
		case 0x4B: cc =  8; CBBIT    (cpu, memory, 1, E); break;
		case 0x4C: cc =  8; CBBIT    (cpu, memory, 1, H); break;
		case 0x4D: cc =  8; CBBIT    (cpu, memory, 1, L); break;
		case 0x4E: cc = 12; CBBITHL  (cpu, memory, 1);    break;
		case 0x4F: cc =  8; CBBIT    (cpu, memory, 1, A); break;

		case 0x50: cc =  8; CBBIT    (cpu, memory, 2, B); break;
		case 0x51: cc =  8; CBBIT    (cpu, memory, 2, C); break;
		case 0x52: cc =  8; CBBIT    (cpu, memory, 2, D); break;
		case 0x53: cc =  8; CBBIT    (cpu, memory, 2, E); break;
		case 0x54: cc =  8; CBBIT    (cpu, memory, 2, H); break;
		case 0x55: cc =  8; CBBIT    (cpu, memory, 2, L); break;
		case 0x56: cc = 12; CBBITHL  (cpu, memory, 2);    break;
		case 0x57: cc =  8; CBBIT    (cpu, memory, 2, A); break;
		case 0x58: cc =  8; CBBIT    (cpu, memory, 3, B); break;
		case 0x59: cc =  8; CBBIT    (cpu, memory, 3, C); break;
		case 0x5A: cc =  8; CBBIT    (cpu, memory, 3, D); break;
		case 0x5B: cc =  8; CBBIT    (cpu, memory, 3, E); break;
		case 0x5C: cc =  8; CBBIT    (cpu, memory, 3, H); break;
		case 0x5D: cc =  8; CBBIT    (cpu, memory, 3, L); break;
		case 0x5E: cc = 12; CBBITHL  (cpu, memory, 3);    break;
		case 0x5F: cc =  8; CBBIT    (cpu, memory, 3, A); break;

		case 0x60: cc =  8; CBBIT    (cpu, memory, 4, B); break;
		case 0x61: cc =  8; CBBIT    (cpu, memory, 4, C); break;
		case 0x62: cc =  8; CBBIT    (cpu, memory, 4, D); break;
		case 0x63: cc =  8; CBBIT    (cpu, memory, 4, E); break;
		case 0x64: cc =  8; CBBIT    (cpu, memory, 4, H); break;
		case 0x65: cc =  8; CBBIT    (cpu, memory, 4, L); break;
		case 0x66: cc = 12; CBBITHL  (cpu, memory, 4);    break;
		case 0x67: cc =  8; CBBIT    (cpu, memory, 4, A); break;
		case 0x68: cc =  8; CBBIT    (cpu, memory, 5, B); break;
		case 0x69: cc =  8; CBBIT    (cpu, memory, 5, C); break;
		case 0x6A: cc =  8; CBBIT    (cpu, memory, 5, D); break;
		case 0x6B: cc =  8; CBBIT    (cpu, memory, 5, E); break;
		case 0x6C: cc =  8; CBBIT    (cpu, memory, 5, H); break;
		case 0x6D: cc =  8; CBBIT    (cpu, memory, 5, L); break;
		case 0x6E: cc = 12; CBBITHL  (cpu, memory, 5);    break;
		case 0x6F: cc =  8; CBBIT    (cpu, memory, 5, A); break;

		case 0x70: cc =  8; CBBIT    (cpu, memory, 6, B); break;
		case 0x71: cc =  8; CBBIT    (cpu, memory, 6, C); break;
		case 0x72: cc =  8; CBBIT    (cpu, memory, 6, D); break;
		case 0x73: cc =  8; CBBIT    (cpu, memory, 6, E); break;
		case 0x74: cc =  8; CBBIT    (cpu, memory, 6, H); break;
		case 0x75: cc =  8; CBBIT    (cpu, memory, 6, L); break;
		case 0x76: cc = 12; CBBITHL  (cpu, memory, 6);    break;
		case 0x77: cc =  8; CBBIT    (cpu, memory, 6, A); break;
		case 0x78: cc =  8; CBBIT    (cpu, memory, 7, B); break;
		case 0x79: cc =  8; CBBIT    (cpu, memory, 7, C); break;
		case 0x7A: cc =  8; CBBIT    (cpu, memory, 7, D); break;
		case 0x7B: cc =  8; CBBIT    (cpu, memory, 7, E); break;
		case 0x7C: cc =  8; CBBIT    (cpu, memory, 7, H); break;
		case 0x7D: cc =  8; CBBIT    (cpu, memory, 7, L); break;
		case 0x7E: cc = 12; CBBITHL  (cpu, memory, 7);    break;
		case 0x7F: cc =  8; CBBIT    (cpu, memory, 7, A); break;

		case 0x80: cc =  8; CBRES    (cpu, memory, 0, B); break;
		case 0x81: cc =  8; CBRES    (cpu, memory, 0, C); break;
		case 0x82: cc =  8; CBRES    (cpu, memory, 0, D); break;
		case 0x83: cc =  8; CBRES    (cpu, memory, 0, E); break;
		case 0x84: cc =  8; CBRES    (cpu, memory, 0, H); break;
		case 0x85: cc =  8; CBRES    (cpu, memory, 0, L); break;
		case 0x86: cc = 16; CBRESHL  (cpu, memory, 0);    break;
		case 0x87: cc =  8; CBRES    (cpu, memory, 0, A); break;
		case 0x88: cc =  8; CBRES    (cpu, memory, 1, B); break;
		case 0x89: cc =  8; CBRES    (cpu, memory, 1, C); break;
		case 0x8A: cc =  8; CBRES    (cpu, memory, 1, D); break;
		case 0x8B: cc =  8; CBRES    (cpu, memory, 1, E); break;
		case 0x8C: cc =  8; CBRES    (cpu, memory, 1, H); break;
		case 0x8D: cc =  8; CBRES    (cpu, memory, 1, L); break;
		case 0x8E: cc = 16; CBRESHL  (cpu, memory, 1);    break;
		case 0x8F: cc =  8; CBRES    (cpu, memory, 1, A); break;

		case 0x90: cc =  8; CBRES    (cpu, memory, 2, B); break;
		case 0x91: cc =  8; CBRES    (cpu, memory, 2, C); break;
		case 0x92: cc =  8; CBRES    (cpu, memory, 2, D); break;
		case 0x93: cc =  8; CBRES    (cpu, memory, 2, E); break;
		case 0x94: cc =  8; CBRES    (cpu, memory, 2, H); break;
		case 0x95: cc =  8; CBRES    (cpu, memory, 2, L); break;
		case 0x96: cc = 16; CBRESHL  (cpu, memory, 2);    break;
		case 0x97: cc =  8; CBRES    (cpu, memory, 2, A); break;
		case 0x98: cc =  8; CBRES    (cpu, memory, 3, B); break;
		case 0x99: cc =  8; CBRES    (cpu, memory, 3, C); break;
		case 0x9A: cc =  8; CBRES    (cpu, memory, 3, D); break;
		case 0x9B: cc =  8; CBRES    (cpu, memory, 3, E); break;
		case 0x9C: cc =  8; CBRES    (cpu, memory, 3, H); break;
		case 0x9D: cc =  8; CBRES    (cpu, memory, 3, L); break;
		case 0x9E: cc = 16; CBRESHL  (cpu, memory, 3);    break;
		case 0x9F: cc =  8; CBRES    (cpu, memory, 3, A); break;

		case 0xA0: cc =  8; CBRES    (cpu, memory, 4, B); break;
		case 0xA1: cc =  8; CBRES    (cpu, memory, 4, C); break;
		case 0xA2: cc =  8; CBRES    (cpu, memory, 4, D); break;
		case 0xA3: cc =  8; CBRES    (cpu, memory, 4, E); break;
		case 0xA4: cc =  8; CBRES    (cpu, memory, 4, H); break;
		case 0xA5: cc =  8; CBRES    (cpu, memory, 4, L); break;
		case 0xA6: cc = 16; CBRESHL  (cpu, memory, 4);    break;
		case 0xA7: cc =  8; CBRES    (cpu, memory, 4, A); break;
		case 0xA8: cc =  8; CBRES    (cpu, memory, 5, B); break;
		case 0xA9: cc =  8; CBRES    (cpu, memory, 5, C); break;
		case 0xAA: cc =  8; CBRES    (cpu, memory, 5, D); break;
		case 0xAB: cc =  8; CBRES    (cpu, memory, 5, E); break;
		case 0xAC: cc =  8; CBRES    (cpu, memory, 5, H); break;
		case 0xAD: cc =  8; CBRES    (cpu, memory, 5, L); break;
		case 0xAE: cc = 16; CBRESHL  (cpu, memory, 5);    break;
		case 0xAF: cc =  8; CBRES    (cpu, memory, 5, A); break;

		case 0xB0: cc =  8; CBRES    (cpu, memory, 6, B); break;
		case 0xB1: cc =  8; CBRES    (cpu, memory, 6, C); break;
		case 0xB2: cc =  8; CBRES    (cpu, memory, 6, D); break;
		case 0xB3: cc =  8; CBRES    (cpu, memory, 6, E); break;
		case 0xB4: cc =  8; CBRES    (cpu, memory, 6, H); break;
		case 0xB5: cc =  8; CBRES    (cpu, memory, 6, L); break;
		case 0xB6: cc = 16; CBRESHL  (cpu, memory, 6);    break;
		case 0xB7: cc =  8; CBRES    (cpu, memory, 6, A); break;
		case 0xB8: cc =  8; CBRES    (cpu, memory, 7, B); break;
		case 0xB9: cc =  8; CBRES    (cpu, memory, 7, C); break;
		case 0xBA: cc =  8; CBRES    (cpu, memory, 7, D); break;
		case 0xBB: cc =  8; CBRES    (cpu, memory, 7, E); break;
		case 0xBC: cc =  8; CBRES    (cpu, memory, 7, H); break;
		case 0xBD: cc =  8; CBRES    (cpu, memory, 7, L); break;
		case 0xBE: cc = 16; CBRESHL  (cpu, memory, 7);    break;
		case 0xBF: cc =  8; CBRES    (cpu, memory, 7, A); break;

		case 0xC0: cc =  8; CBSET    (cpu, memory, 0, B); break;
		case 0xC1: cc =  8; CBSET    (cpu, memory, 0, C); break;
		case 0xC2: cc =  8; CBSET    (cpu, memory, 0, D); break;
		case 0xC3: cc =  8; CBSET    (cpu, memory, 0, E); break;
		case 0xC4: cc =  8; CBSET    (cpu, memory, 0, H); break;
		case 0xC5: cc =  8; CBSET    (cpu, memory, 0, L); break;
		case 0xC6: cc = 16; CBSETHL  (cpu, memory, 0);    break;
		case 0xC7: cc =  8; CBSET    (cpu, memory, 0, A); break;
		case 0xC8: cc =  8; CBSET    (cpu, memory, 1, B); break;
		case 0xC9: cc =  8; CBSET    (cpu, memory, 1, C); break;
		case 0xCA: cc =  8; CBSET    (cpu, memory, 1, D); break;
		case 0xCB: cc =  8; CBSET    (cpu, memory, 1, E); break;
		case 0xCC: cc =  8; CBSET    (cpu, memory, 1, H); break;
		case 0xCD: cc =  8; CBSET    (cpu, memory, 1, L); break;
		case 0xCE: cc = 16; CBSETHL  (cpu, memory, 1);    break;
		case 0xCF: cc =  8; CBSET    (cpu, memory, 1, A); break;

		case 0xD0: cc =  8; CBSET    (cpu, memory, 2, B); break;
		case 0xD1: cc =  8; CBSET    (cpu, memory, 2, C); break;
		case 0xD2: cc =  8; CBSET    (cpu, memory, 2, D); break;
		case 0xD3: cc =  8; CBSET    (cpu, memory, 2, E); break;
		case 0xD4: cc =  8; CBSET    (cpu, memory, 2, H); break;
		case 0xD5: cc =  8; CBSET    (cpu, memory, 2, L); break;
		case 0xD6: cc = 16; CBSETHL  (cpu, memory, 2);    break;
		case 0xD7: cc =  8; CBSET    (cpu, memory, 2, A); break;
		case 0xD8: cc =  8; CBSET    (cpu, memory, 3, B); break;
		case 0xD9: cc =  8; CBSET    (cpu, memory, 3, C); break;
		case 0xDA: cc =  8; CBSET    (cpu, memory, 3, D); break;
		case 0xDB: cc =  8; CBSET    (cpu, memory, 3, E); break;
		case 0xDC: cc =  8; CBSET    (cpu, memory, 3, H); break;
		case 0xDD: cc =  8; CBSET    (cpu, memory, 3, L); break;
		case 0xDE: cc = 16; CBSETHL  (cpu, memory, 3);    break;
		case 0xDF: cc =  8; CBSET    (cpu, memory, 3, A); break;

		case 0xE0: cc =  8; CBSET    (cpu, memory, 4, B); break;
		case 0xE1: cc =  8; CBSET    (cpu, memory, 4, C); break;
		case 0xE2: cc =  8; CBSET    (cpu, memory, 4, D); break;
		case 0xE3: cc =  8; CBSET    (cpu, memory, 4, E); break;
		case 0xE4: cc =  8; CBSET    (cpu, memory, 4, H); break;
		case 0xE5: cc =  8; CBSET    (cpu, memory, 4, L); break;
		case 0xE6: cc = 16; CBSETHL  (cpu, memory, 4);    break;
		case 0xE7: cc =  8; CBSET    (cpu, memory, 4, A); break;
		case 0xE8: cc =  8; CBSET    (cpu, memory, 5, B); break;
		case 0xE9: cc =  8; CBSET    (cpu, memory, 5, C); break;
		case 0xEA: cc =  8; CBSET    (cpu, memory, 5, D); break;
		case 0xEB: cc =  8; CBSET    (cpu, memory, 5, E); break;
		case 0xEC: cc =  8; CBSET    (cpu, memory, 5, H); break;
		case 0xED: cc =  8; CBSET    (cpu, memory, 5, L); break;
		case 0xEE: cc = 16; CBSETHL  (cpu, memory, 5);    break;
		case 0xEF: cc =  8; CBSET    (cpu, memory, 5, A); break;

		case 0xF0: cc =  8; CBSET    (cpu, memory, 6, B); break;
		case 0xF1: cc =  8; CBSET    (cpu, memory, 6, C); break;
		case 0xF2: cc =  8; CBSET    (cpu, memory, 6, D); break;
		case 0xF3: cc =  8; CBSET    (cpu, memory, 6, E); break;
		case 0xF4: cc =  8; CBSET    (cpu, memory, 6, H); break;
		case 0xF5: cc =  8; CBSET    (cpu, memory, 6, L); break;
		case 0xF6: cc = 16; CBSETHL  (cpu, memory, 6);    break;
		case 0xF7: cc =  8; CBSET    (cpu, memory, 6, A); break;
		case 0xF8: cc =  8; CBSET    (cpu, memory, 7, B); break;
		case 0xF9: cc =  8; CBSET    (cpu, memory, 7, C); break;
		case 0xFA: cc =  8; CBSET    (cpu, memory, 7, D); break;
		case 0xFB: cc =  8; CBSET    (cpu, memory, 7, E); break;
		case 0xFC: cc =  8; CBSET    (cpu, memory, 7, H); break;
		case 0xFD: cc =  8; CBSET    (cpu, memory, 7, L); break;
		case 0xFE: cc = 16; CBSETHL  (cpu, memory, 7);    break;
		case 0xFF: cc =  8; CBSET    (cpu, memory, 7, A); break;
		default: cc=-1; assert(0);
	}
	cpu->pc += 2;

	return cc;
}

int step_instruction(
		Cpu* cpu,
		Memory* memory,
		Machine* machine,
		instruction_error_fn fail,
		void* fail_data)
{
	u8 opcode = read_memory(memory, cpu->pc);

	/*
	printf("%04x: %02x    AF=%02x%02x BC=%02x%02x DE=%02x%02x HL=%02x%02x    IE:%d%d%d%d%d%d%d%d SCX:%02x SCY:%02x\n",
			(int)cpu->pc, (int)opcode,
			(int)cpu->a, (int)cpu->f,
			(int)cpu->b, (int)cpu->c,
			(int)cpu->d, (int)cpu->e,
			(int)cpu->h, (int)cpu->l,
			(memory->ram[0xFFFF] & (1 << 7)) > 0,
			(memory->ram[0xFFFF] & (1 << 6)) > 0,
			(memory->ram[0xFFFF] & (1 << 5)) > 0,
			(memory->ram[0xFFFF] & (1 << 4)) > 0,
			(memory->ram[0xFFFF] & (1 << 4)) > 0,
			(memory->ram[0xFFFF] & (1 << 2)) > 0,
			(memory->ram[0xFFFF] & (1 << 1)) > 0,
			(memory->ram[0xFFFF] & (1 << 0)) > 0,
			(int)memory->ram[0xFF43],
			(int)memory->ram[0xFF42]);
	*/

	/*
	u8 n = read_memory(memory, cpu->pc + 1);
	u8 np = read_memory(memory, cpu->pc + 2);
	u16 nn = join_word_16(np, n);
	*/

	int cc; /* cycle count for current instruction */

	switch(opcode)
	{
		case 0x00: cc =  4; NOP    (cpu, memory);            break;
		case 0x01: cc = 12; LD2    (cpu, memory, BC);        break;
		case 0x02: cc =  8; LDPR   (cpu, memory, BC);        break;
		case 0x03: cc =  8; INC2   (cpu, memory, BC);        break;
		case 0x04: cc =  4; INC    (cpu, memory, B);         break;
		case 0x05: cc =  4; DEC    (cpu, memory, B);         break;
		case 0x06: cc =  8; LDRN   (cpu, memory, B);         break;
		case 0x07: cc =  4; RLCA   (cpu, memory);            break;
		case 0x08: cc = 20; LDP2   (cpu, memory);            break;
		case 0x09: cc =  8; ADD2HL (cpu, memory, BC);        break;
		case 0x0A: cc =  8; LDAP   (cpu, memory, BC);        break;
		case 0x0B: cc =  8; DEC2   (cpu, memory, BC);        break;
		case 0x0C: cc =  4; INC    (cpu, memory, C);         break;
		case 0x0D: cc =  4; DEC    (cpu, memory, C);         break;
		case 0x0E: cc =  8; LDRN   (cpu, memory, C);         break;
		case 0x0F: cc =  4; RRCA   (cpu, memory);            break;

		case 0x10: cc =  4; STOP   (cpu, memory);            break;
		case 0x11: cc = 12; LD2    (cpu, memory, DE);        break;
		case 0x12: cc =  8; LDPR   (cpu, memory, DE);        break;
		case 0x13: cc =  8; INC2   (cpu, memory, DE);        break;
		case 0x14: cc =  4; INC    (cpu, memory, D);         break;
		case 0x15: cc =  4; DEC    (cpu, memory, D);         break;
		case 0x16: cc =  8; LDRN   (cpu, memory, D);         break;
		case 0x17: cc =  4; RLA    (cpu, memory);            break;
		case 0x18: cc =  8; JR     (cpu, memory, FLAG_NONE); break;
		case 0x19: cc =  8; ADD2HL (cpu, memory, DE);        break;
		case 0x1A: cc =  8; LDAP   (cpu, memory, DE);        break;
		case 0x1B: cc =  8; DEC2   (cpu, memory, DE);        break;
		case 0x1C: cc =  4; INC    (cpu, memory, E);         break;
		case 0x1D: cc =  4; DEC    (cpu, memory, E);         break;
		case 0x1E: cc =  8; LDRN   (cpu, memory, E);         break;
		case 0x1F: cc =  4; RRA    (cpu, memory);            break;

		case 0x20: cc =  8; JR     (cpu, memory, FLAG_NZ);   break;
		case 0x21: cc = 12; LD2    (cpu, memory, HL);        break;
		case 0x22: cc =  8; LDIM   (cpu, memory);            break;
		case 0x23: cc =  8; INC2   (cpu, memory, HL);        break;
		case 0x24: cc =  4; INC    (cpu, memory, H);         break;
		case 0x25: cc =  4; DEC    (cpu, memory, H);         break;
		case 0x26: cc =  8; LDRN   (cpu, memory, H);         break;
		case 0x27: cc =  4; DAA    (cpu, memory);            break;
		case 0x28: cc =  8; JR     (cpu, memory, FLAG_Z);    break;
		case 0x29: cc =  8; ADD2HL (cpu, memory, HL);        break;
		case 0x2A: cc =  8; LDIR   (cpu, memory);            break;
		case 0x2B: cc =  8; DEC2   (cpu, memory, HL);        break;
		case 0x2C: cc =  4; INC    (cpu, memory, L);         break;
		case 0x2D: cc =  4; DEC    (cpu, memory, L);         break;
		case 0x2E: cc =  8; LDRN   (cpu, memory, L);         break;
		case 0x2F: cc =  4; CPL    (cpu, memory);            break;

		case 0x30: cc =  8; JR     (cpu, memory, FLAG_NC);   break;
		case 0x31: cc = 12; LD2    (cpu, memory, SP);        break;
		case 0x32: cc =  8; LDDM   (cpu, memory);            break;
		case 0x33: cc =  8; INC2   (cpu, memory, SP);        break;
		case 0x34: cc = 12; INCHL  (cpu, memory);            break;
		case 0x35: cc = 12; DECHL  (cpu, memory);            break;
		case 0x36: cc = 12; LDHLN  (cpu, memory);            break;
		case 0x37: cc =  4; SCF    (cpu, memory);            break;
		case 0x38: cc =  8; JR     (cpu, memory, FLAG_C);    break;
		case 0x39: cc =  8; ADD2HL (cpu, memory, SP);        break;
		case 0x3A: cc =  8; LDDR   (cpu, memory);            break;
		case 0x3B: cc =  8; DEC2   (cpu, memory, SP);        break;
		case 0x3C: cc =  4; INC    (cpu, memory, A);         break;
		case 0x3D: cc =  4; DEC    (cpu, memory, A);         break;
		case 0x3E: cc =  8; LDRN   (cpu, memory, A);         break;
		case 0x3F: cc =  4; CCF    (cpu, memory);            break;

		case 0x40: cc =  4; LDR    (cpu, memory, B, B);      break;
		case 0x41: cc =  4; LDR    (cpu, memory, B, C);      break;
		case 0x42: cc =  4; LDR    (cpu, memory, B, D);      break;
		case 0x43: cc =  4; LDR    (cpu, memory, B, E);      break;
		case 0x44: cc =  4; LDR    (cpu, memory, B, H);      break;
		case 0x45: cc =  4; LDR    (cpu, memory, B, L);      break;
		case 0x46: cc =  8; LDRHL  (cpu, memory,  B);        break;
		case 0x47: cc =  4; LDR    (cpu, memory, B, A);      break;
		case 0x48: cc =  4; LDR    (cpu, memory, C, B);      break;
		case 0x49: cc =  4; LDR    (cpu, memory, C, C);      break;
		case 0x4A: cc =  4; LDR    (cpu, memory, C, D);      break;
		case 0x4B: cc =  4; LDR    (cpu, memory, C, E);      break;
		case 0x4C: cc =  4; LDR    (cpu, memory, C, H);      break;
		case 0x4D: cc =  4; LDR    (cpu, memory, C, L);      break;
		case 0x4E: cc =  8; LDRHL  (cpu, memory, C);         break;
		case 0x4F: cc =  4; LDR    (cpu, memory, C, A);      break;

		case 0x50: cc =  4; LDR    (cpu, memory, D, B);      break;
		case 0x51: cc =  4; LDR    (cpu, memory, D, C);      break;
		case 0x52: cc =  4; LDR    (cpu, memory, D, D);      break;
		case 0x53: cc =  4; LDR    (cpu, memory, D, E);      break;
		case 0x54: cc =  4; LDR    (cpu, memory, D, H);      break;
		case 0x55: cc =  4; LDR    (cpu, memory, D, L);      break;
		case 0x56: cc =  8; LDRHL  (cpu, memory,  D);        break;
		case 0x57: cc =  4; LDR    (cpu, memory, D, A);      break;
		case 0x58: cc =  4; LDR    (cpu, memory, E, B);      break;
		case 0x59: cc =  4; LDR    (cpu, memory, E, C);      break;
		case 0x5A: cc =  4; LDR    (cpu, memory, E, D);      break;
		case 0x5B: cc =  4; LDR    (cpu, memory, E, E);      break;
		case 0x5C: cc =  4; LDR    (cpu, memory, E, H);      break;
		case 0x5D: cc =  4; LDR    (cpu, memory, E, L);      break;
		case 0x5E: cc =  8; LDRHL  (cpu, memory, E);         break;
		case 0x5F: cc =  4; LDR    (cpu, memory, E, A);      break;

		case 0x60: cc =  4; LDR    (cpu, memory, H, B);      break;
		case 0x61: cc =  4; LDR    (cpu, memory, H, C);      break;
		case 0x62: cc =  4; LDR    (cpu, memory, H, D);      break;
		case 0x63: cc =  4; LDR    (cpu, memory, H, E);      break;
		case 0x64: cc =  4; LDR    (cpu, memory, H, H);      break;
		case 0x65: cc =  4; LDR    (cpu, memory, H, L);      break;
		case 0x66: cc =  8; LDRHL  (cpu, memory,  H);        break;
		case 0x67: cc =  4; LDR    (cpu, memory, H, A);      break;
		case 0x68: cc =  4; LDR    (cpu, memory, L, B);      break;
		case 0x69: cc =  4; LDR    (cpu, memory, L, C);      break;
		case 0x6A: cc =  4; LDR    (cpu, memory, L, D);      break;
		case 0x6B: cc =  4; LDR    (cpu, memory, L, E);      break;
		case 0x6C: cc =  4; LDR    (cpu, memory, L, H);      break;
		case 0x6D: cc =  4; LDR    (cpu, memory, L, L);      break;
		case 0x6E: cc =  8; LDRHL  (cpu, memory, L);         break;
		case 0x6F: cc =  4; LDR    (cpu, memory, L, A);      break;

		case 0x70: cc =  8; LDHL   (cpu, memory, B);         break;
		case 0x71: cc =  8; LDHL   (cpu, memory, C);         break;
		case 0x72: cc =  8; LDHL   (cpu, memory, D);         break;
		case 0x73: cc =  8; LDHL   (cpu, memory, E);         break;
		case 0x74: cc =  8; LDHL   (cpu, memory, H);         break;
		case 0x75: cc =  8; LDHL   (cpu, memory, L);         break;
		case 0x76: cc =  4; HALT   (cpu, memory);            break;
		case 0x77: cc =  8; LDHL   (cpu, memory, A);         break;
		case 0x78: cc =  4; LDR    (cpu, memory, A, B);      break;
		case 0x79: cc =  4; LDR    (cpu, memory, A, C);      break;
		case 0x7A: cc =  4; LDR    (cpu, memory, A, D);      break;
		case 0x7B: cc =  4; LDR    (cpu, memory, A, E);      break;
		case 0x7C: cc =  4; LDR    (cpu, memory, A, H);      break;
		case 0x7D: cc =  4; LDR    (cpu, memory, A, L);      break;
		case 0x7E: cc =  8; LDRHL  (cpu, memory, A);         break;
		case 0x7F: cc =  4; LDR    (cpu, memory, A, A);      break;

		case 0x80: cc =  4; ADD    (cpu, memory, B);         break;
		case 0x81: cc =  4; ADD    (cpu, memory, C);         break;
		case 0x82: cc =  4; ADD    (cpu, memory, D);         break;
		case 0x83: cc =  4; ADD    (cpu, memory, E);         break;
		case 0x84: cc =  4; ADD    (cpu, memory, H);         break;
		case 0x85: cc =  4; ADD    (cpu, memory, L);         break;
		case 0x86: cc =  8; ADDHL  (cpu, memory);            break;
		case 0x87: cc =  4; ADD    (cpu, memory, A);         break;
		case 0x88: cc =  4; ADC    (cpu, memory, B);         break;
		case 0x89: cc =  4; ADC    (cpu, memory, C);         break;
		case 0x8A: cc =  4; ADC    (cpu, memory, D);         break;
		case 0x8B: cc =  4; ADC    (cpu, memory, E);         break;
		case 0x8C: cc =  4; ADC    (cpu, memory, H);         break;
		case 0x8D: cc =  4; ADC    (cpu, memory, L);         break;
		case 0x8E: cc =  8; ADCHL  (cpu, memory);            break;
		case 0x8F: cc =  4; ADC    (cpu, memory, A);         break;

		case 0x90: cc =  4; SUB    (cpu, memory, B);         break;
		case 0x91: cc =  4; SUB    (cpu, memory, C);         break;
		case 0x92: cc =  4; SUB    (cpu, memory, D);         break;
		case 0x93: cc =  4; SUB    (cpu, memory, E);         break;
		case 0x94: cc =  4; SUB    (cpu, memory, H);         break;
		case 0x95: cc =  4; SUB    (cpu, memory, L);         break;
		case 0x96: cc =  8; SUBHL  (cpu, memory);            break;
		case 0x97: cc =  4; SUB    (cpu, memory, A);         break;
		case 0x98: cc =  4; SBC    (cpu, memory, B);         break;
		case 0x99: cc =  4; SBC    (cpu, memory, C);         break;
		case 0x9A: cc =  4; SBC    (cpu, memory, D);         break;
		case 0x9B: cc =  4; SBC    (cpu, memory, E);         break;
		case 0x9C: cc =  4; SBC    (cpu, memory, H);         break;
		case 0x9D: cc =  4; SBC    (cpu, memory, L);         break;
		case 0x9E: cc =  8; SBCHL  (cpu, memory);            break;
		case 0x9F: cc =  4; SBC    (cpu, memory, A);         break;

		case 0xA0: cc =  4; AND    (cpu, memory, B);         break;
		case 0xA1: cc =  4; AND    (cpu, memory, C);         break;
		case 0xA2: cc =  4; AND    (cpu, memory, D);         break;
		case 0xA3: cc =  4; AND    (cpu, memory, E);         break;
		case 0xA4: cc =  4; AND    (cpu, memory, H);         break;
		case 0xA5: cc =  4; AND    (cpu, memory, L);         break;
		case 0xA6: cc =  8; ANDHL  (cpu, memory);            break;
		case 0xA7: cc =  4; AND    (cpu, memory, A);         break;
		case 0xA8: cc =  4; XOR    (cpu, memory, B);         break;
		case 0xA9: cc =  4; XOR    (cpu, memory, C);         break;
		case 0xAA: cc =  4; XOR    (cpu, memory, D);         break;
		case 0xAB: cc =  4; XOR    (cpu, memory, E);         break;
		case 0xAC: cc =  4; XOR    (cpu, memory, H);         break;
		case 0xAD: cc =  4; XOR    (cpu, memory, L);         break;
		case 0xAE: cc =  8; XORHL  (cpu, memory);            break;
		case 0xAF: cc =  4; XOR    (cpu, memory, A);         break;

		case 0xB0: cc =  4; OR     (cpu, memory, B);         break;
		case 0xB1: cc =  4; OR     (cpu, memory, C);         break;
		case 0xB2: cc =  4; OR     (cpu, memory, D);         break;
		case 0xB3: cc =  4; OR     (cpu, memory, E);         break;
		case 0xB4: cc =  4; OR     (cpu, memory, H);         break;
		case 0xB5: cc =  4; OR     (cpu, memory, L);         break;
		case 0xB6: cc =  8; ORHL   (cpu, memory);            break;
		case 0xB7: cc =  4; OR     (cpu, memory, A);         break;
		case 0xB8: cc =  4; CP     (cpu, memory, B);         break;
		case 0xB9: cc =  4; CP     (cpu, memory, C);         break;
		case 0xBA: cc =  4; CP     (cpu, memory, D);         break;
		case 0xBB: cc =  4; CP     (cpu, memory, E);         break;
		case 0xBC: cc =  4; CP     (cpu, memory, H);         break;
		case 0xBD: cc =  4; CP     (cpu, memory, L);         break;
		case 0xBE: cc =  8; CPHL   (cpu, memory);            break;
		case 0xBF: cc =  4; CP     (cpu, memory, A);         break;

		case 0xC0: cc =  8; RET    (cpu, memory, FLAG_NZ);   break;
		case 0xC1: cc = 12; POP    (cpu, memory, SR_BC);     break;
		case 0xC2: cc = 12; JP     (cpu, memory, FLAG_NZ);   break;
		case 0xC3: cc = 12; JP     (cpu, memory, FLAG_NONE); break;
		case 0xC4: cc = 12; CALL   (cpu, memory, FLAG_NZ);   break;
		case 0xC5: cc = 16; PUSH   (cpu, memory, SR_BC);     break;
		case 0xC6: cc =  8; ADDN   (cpu, memory);            break;
		case 0xC7: cc = 32; RST    (cpu, memory, 0x0000);    break;
		case 0xC8: cc =  8; RET    (cpu, memory, FLAG_Z);    break;
		case 0xC9: cc =  8; RET    (cpu, memory, FLAG_NONE); break;
		case 0xCA: cc = 12; JP     (cpu, memory, FLAG_Z);    break;
		case 0xCB: cc =     CB     (cpu, memory);            break;
		case 0xCC: cc = 12; CALL   (cpu, memory, FLAG_Z);    break;
		case 0xCD: cc = 12; CALL   (cpu, memory, FLAG_NONE); break;
		case 0xCE: cc =  8; ADCN   (cpu, memory);            break;
		case 0xCF: cc = 32; RST    (cpu, memory, 0x0008);    break;

		case 0xD0: cc =  8; RET    (cpu, memory, FLAG_NC);   break;
		case 0xD1: cc = 12; POP    (cpu, memory, SR_DE);     break;
		case 0xD2: cc = 12; JP     (cpu, memory, FLAG_NC);   break;
		case 0xD3: cc=-1; fail(fail_data); break;
		case 0xD4: cc = 12; CALL   (cpu, memory, FLAG_NC);   break;
		case 0xD5: cc = 16; PUSH   (cpu, memory, SR_DE);     break;
		case 0xD6: cc =  8; SUBN   (cpu, memory);            break;
		case 0xD7: cc = 32; RST    (cpu, memory, 0x0010);    break;
		case 0xD8: cc =  8; RET    (cpu, memory, FLAG_C);    break;
		case 0xD9:
			cc =  8; RETI   (cpu, memory);
			machine->ime = 1;
			break;
		case 0xDA: cc = 12; JP     (cpu, memory, FLAG_C);    break;
		case 0xDB: cc=-1; fail(fail_data); break;
		case 0xDC: cc = 12; CALL   (cpu, memory, FLAG_C);    break;
		case 0xDD: cc=-1; fail(fail_data); break;
		case 0xDE: cc =  8; SBCN   (cpu, memory);            break;
		case 0xDF: cc = 32; RST    (cpu, memory, 0x0018);    break;

		case 0xE0: cc = 12; LDHM   (cpu, memory);            break;
		case 0xE1: cc = 12; POP    (cpu, memory, SR_HL);     break;
		case 0xE2: cc =  8; LDPC   (cpu, memory);            break;
		case 0xE3: cc=-1; fail(fail_data); break;
		case 0xE4: cc=-1; fail(fail_data); break;
		case 0xE5: cc = 16; PUSH   (cpu, memory, SR_HL);     break;
		case 0xE6: cc =  8; ANDN   (cpu, memory);            break;
		case 0xE7: cc = 32; RST    (cpu, memory, 0x0020);    break;
		case 0xE8: cc = 16; ADD2SP (cpu, memory);            break;
		case 0xE9: cc =  4; JPHL   (cpu, memory);            break;
		case 0xEA: cc = 16; LDPN   (cpu, memory);            break;
		case 0xEB: cc=-1; fail(fail_data); break;
		case 0xEC: cc=-1; fail(fail_data); break;
		case 0xED: cc=-1; fail(fail_data); break;
		case 0xEE: cc =  8; XORN   (cpu, memory);            break;
		case 0xEF: cc = 32; RST    (cpu, memory, 0x0028);    break;

		case 0xF0: cc = 12; LDHR   (cpu, memory);            break;
		case 0xF1: cc = 12; POP    (cpu, memory, SR_AF);     break;
		case 0xF2: cc =  8; LDAC   (cpu, memory);            break;
		case 0xF3:
			cc =  4; DI     (cpu, memory);
			machine->ime = 0;
			break;
		case 0xF4: cc=-1; fail(fail_data); break;
		case 0xF5: cc = 16; PUSH   (cpu, memory, SR_AF);     break;
		case 0xF6: cc =  8; ORN    (cpu, memory);            break;
		case 0xF7: cc = 32; RST    (cpu, memory, 0x0030);    break;
		case 0xF8: cc = 12; LDHL2  (cpu, memory);            break;
		case 0xF9: cc =  8; LDSP2  (cpu, memory);            break;
		case 0xFA: cc = 16; LDAPN  (cpu, memory);            break;
		case 0xFB:
			cc =  4; EI     (cpu, memory);
			machine->ime = 1;
			break;
		case 0xFC: cc=-1; fail(fail_data); break;
		case 0xFD: cc=-1; fail(fail_data); break;
		case 0xFE: cc =  8; CPN    (cpu, memory);            break;
		case 0xFF: cc = 32; RST    (cpu, memory, 0x0038);    break;
		default: cc=-1; assert(0);
	}
	return cc;
}

