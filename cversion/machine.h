#ifndef MACHINE_H__
#define MACHINE_H__

#include "util.h"
#include "memory.h"
#include "cpu.h"
#include "breakpoints.h"

#define LCD_WIDTH 160
#define LCD_HEIGHT 144

#define VBLANK_PERIOD 70224
#define HBLANK_PERIOD 456

#define DIV_PERIOD 256

typedef struct Machine_
{
	Cpu* cpu;
	Memory* memory;

	int ime;
	int vblank_counter;
	int hblank_counter;
	int hblank_mode3_counter;
	int hblank_mode0_counter;
	int current_scanline;
	int div_counter;

	u8* display;
} Machine;

Cpu initial_cpu_state();
void initial_memory_state(Memory* memory);

int init_machine(Machine* machine, Cpu* cpu, Memory* memory);
void free_machine(Machine* machine);

int update_machine(Machine* machine, u8* breakpoint_table);

void update_machine_vblank(Machine* machine, u8* breakpoint_table);

#endif

