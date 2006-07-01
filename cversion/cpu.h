#ifndef CPU_H__
#define CPU_H__

#include <assert.h>

#include "util.h"
#include "memory.h"

enum Register { A, B, C, D, E, H, L };
enum RegisterPair { BC, DE, HL, SP };
enum StackRegister { SR_AF, SR_BC, SR_DE, SR_HL };
enum FlagCondition { FLAG_NONE, FLAG_C, FLAG_NC, FLAG_NZ, FLAG_Z };

typedef struct
{
	u8 a;
	u8 b;
	u8 c;
	u8 d;
	u8 e;
	u8 f;
	u8 h;
	u8 l;
	u16 pc;
	u16 sp;
} Cpu;

typedef void(*instruction_error_fn)(void*);

struct Memory_;
struct Machine_;

int step_instruction(Cpu* cpu, struct Memory_* memory, struct Machine_* machine, instruction_error_fn fail, void* fail_data);

inline static u8 read_register(Cpu* cpu, enum Register r)
{
	switch(r)
	{
		case A: return cpu->a;
		case B: return cpu->b;
		case C: return cpu->c;
		case D: return cpu->d;
		case E: return cpu->e;
		case H: return cpu->h;
		case L: return cpu->l;
	}
	assert(0);
}

inline static void write_register(Cpu* cpu, enum Register r, u8 value)
{
	switch(r)
	{
		case A: cpu->a = value; return;
		case B: cpu->b = value; return;
		case C: cpu->c = value; return;
		case D: cpu->d = value; return;
		case E: cpu->e = value; return;
		case H: cpu->h = value; return;
		case L: cpu->l = value; return;
	}
}

inline static u16 read_register_pair(Cpu* cpu, enum RegisterPair rp)
{
	switch(rp)
	{
		case BC: return join_word_16(cpu->b, cpu->c);
		case DE: return join_word_16(cpu->d, cpu->e);
		case HL: return join_word_16(cpu->h, cpu->l);
		case SP: return cpu->sp;
	}
	assert(0);
}

inline static u16 read_stack_register_pair(Cpu* cpu, enum StackRegister sr)
{
	switch(sr)
	{
		case SR_AF: return join_word_16(cpu->a, cpu->f);
		case SR_BC: return join_word_16(cpu->b, cpu->c);
		case SR_DE: return join_word_16(cpu->d, cpu->e);
		case SR_HL: return join_word_16(cpu->h, cpu->l);
	}
	assert(0);
}

inline static void write_register_pair(Cpu* cpu, enum RegisterPair rp, u16 value)
{
	switch(rp)
	{
		case BC: split_word_16(value, &(cpu->b), &(cpu->c)); return;
		case DE: split_word_16(value, &(cpu->d), &(cpu->e)); return;
		case HL: split_word_16(value, &(cpu->h), &(cpu->l)); return;
		case SP: cpu->sp = value; return;
	}
}

inline static void write_stack_register_pair(Cpu* cpu, enum StackRegister sr, u16 value)
{
	switch(sr)
	{
		case SR_AF: split_word_16(value, &(cpu->a), &(cpu->f)); return;
		case SR_BC: split_word_16(value, &(cpu->b), &(cpu->c)); return;
		case SR_DE: split_word_16(value, &(cpu->d), &(cpu->e)); return;
		case SR_HL: split_word_16(value, &(cpu->h), &(cpu->l)); return;
	}
}

inline static void write_flags(Cpu* cpu, int z, int n, int h, int c)
{
	if(z == 0)
		cpu->f &= ~(1 << 7);
	else if(z > 0)
		cpu->f |= (1 << 7);
	if(n == 0)
		cpu->f &= ~(1 << 6);
	else if(n > 0)
		cpu->f |= (1 << 6);
	if(h == 0)
		cpu->f &= ~(1 << 5);
	else if(h > 0)
		cpu->f |= (1 << 5);
	if(c == 0)
		cpu->f &= ~(1 << 4);
	else if(c > 0)
		cpu->f |= (1 << 4);
}

#endif

