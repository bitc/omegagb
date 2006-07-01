#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "machine.h"
#include "cpu.h"

Cpu initial_cpu_state()
{
	Cpu cpu;
	cpu.a = 0x01;
	cpu.b = 0x00;
	cpu.c = 0x13;
	cpu.d = 0x00;
	cpu.e = 0xD8;
	cpu.f = 0xB0;
	cpu.h = 0x01;
	cpu.l = 0x4D;
	cpu.pc = 0x0100;
	cpu.sp = 0xFFFE;

	return cpu;
}

void initial_memory_state(Memory* memory)
{
	memory->ram[0xFF05] = 0x00;
	memory->ram[0xFF06] = 0x00;
	memory->ram[0xFF07] = 0x00;
	memory->ram[0xFF10] = 0x80;
	memory->ram[0xFF11] = 0xBF;
	memory->ram[0xFF12] = 0xF3;
	memory->ram[0xFF14] = 0xBF;
	memory->ram[0xFF16] = 0x3F;
	memory->ram[0xFF17] = 0x00;
	memory->ram[0xFF19] = 0xBF;
	memory->ram[0xFF1A] = 0x7F;
	memory->ram[0xFF1B] = 0xFF;
	memory->ram[0xFF1C] = 0x9F;
	memory->ram[0xFF1E] = 0xBF;
	memory->ram[0xFF20] = 0xFF;
	memory->ram[0xFF21] = 0x00;
	memory->ram[0xFF22] = 0x00;
	memory->ram[0xFF23] = 0xBF;
	memory->ram[0xFF24] = 0x77;
	memory->ram[0xFF25] = 0xF3;
	memory->ram[0xFF26] = 0xF1;
	memory->ram[0xFF40] = 0x91;
	memory->ram[0xFF42] = 0x00;
	memory->ram[0xFF43] = 0x00;
	memory->ram[0xFF45] = 0x00;
	memory->ram[0xFF47] = 0xFC;
	memory->ram[0xFF48] = 0xFF;
	memory->ram[0xFF49] = 0xFF;
	memory->ram[0xFF4A] = 0x00;
	memory->ram[0xFF4B] = 0x00;
	memory->ram[0xFFFF] = 0x00;
}

int init_machine(Machine* machine, Cpu* cpu, Memory* memory)
{
	machine->cpu = cpu;
	machine->memory = memory;

	*(machine->cpu) = initial_cpu_state();
	initial_memory_state(machine->memory);

	machine->ime = 0;
	machine->vblank_counter = 0;
	machine->hblank_counter = 0;
	machine->hblank_mode3_counter = 80;
	machine->hblank_mode0_counter = 80 + 172;
	machine->current_scanline = 153;
	machine->div_counter = 0;

	machine->display = malloc(LCD_WIDTH * LCD_HEIGHT);
	if(machine->display == 0)
		return -1;

	memset(machine->display, 0, LCD_WIDTH * LCD_HEIGHT);

	return 0;
}

void free_machine(Machine* machine)
{
	free(machine->display);
}

void instruction_error_handler(void* data)
{
	fprintf(stderr, "INVALID CPU INSTRUCTION\n");
	abort();
}

inline static void render_scanline(Machine* machine)
{
	int scanline = machine->current_scanline;

	int scx = machine->memory->ram[0xFF43];
	int scy = machine->memory->ram[0xFF42];

	int lcdc = machine->memory->ram[0xFF40];

	/*
	printf("%3d LCDC: %d%d%d%d%d%d%d%d\n", scanline,
			(lcdc&(1<<7))>0,
			(lcdc&(1<<6))>0,
			(lcdc&(1<<5))>0,
			(lcdc&(1<<4))>0,
			(lcdc&(1<<3))>0,
			(lcdc&(1<<2))>0,
			(lcdc&(1<<1))>0,
			(lcdc&(1<<0))>0);
	*/

	if((lcdc & (1 << 7)) == 0)
	{
		int x;
		for(x = 0; x < LCD_WIDTH; x++)
			machine->display[scanline * LCD_WIDTH + x] = 0;
		return;
	}

	int bgmap_start_addr = (lcdc & (1 << 3)) ? 0x9C00 : 0x9800;

	int bgtiles_start_addr = (lcdc & (1 << 4)) ? 0x8000 : 0x9000;

	int x;
	int y = scanline;
	for(x = 0; x < LCD_WIDTH; x++)
	{
		int xp = (x + scx) % 256;
		int yp = (y + scy) % 256;
		int xrow = xp / 8;
		int yrow = yp / 8;
		int tile_num = yrow * 32 + xrow;
		int tile_index = machine->memory->ram[tile_num + bgmap_start_addr];
		int tile_start_mem = bgtiles_start_addr + (16 * tile_index);
		int xoff = 7 - (xp % 8);
		int yoff = yp % 8;
		int hi_byte = tile_start_mem + (yoff * 2);
		int lo_byte = tile_start_mem + (yoff * 2) + 1;
		int hi_byte_value = machine->memory->ram[hi_byte];
		int lo_byte_value = machine->memory->ram[lo_byte];
		int color = 2*((lo_byte_value & (1 << xoff)) > 0) + ((hi_byte_value & (1 << xoff)) > 0);
		machine->display[y * LCD_WIDTH + x] = color;
	}
}

/* returns 1 if a vblank occured
 * otherwise returns 0 */
inline static int irq_update(Machine* machine, int cycles)
{
	int vblank_flag = 0;

	machine->vblank_counter -= cycles;
	machine->hblank_counter -= cycles;
	machine->hblank_mode3_counter -= cycles;
	machine->hblank_mode0_counter -= cycles;
	machine->div_counter -= cycles;

	if(machine->hblank_counter <= 0)
	{
		machine->hblank_counter += HBLANK_PERIOD;
		machine->memory->ram[0xFF44] = (machine->memory->ram[0xFF44] + 1) % 154;
		machine->current_scanline = (machine->current_scanline + 1) % 154;
		if (machine->current_scanline < 144)
		{
			machine->memory->ram[0xFF41] &= ~(1 << 0);
			machine->memory->ram[0xFF41] |= (1 << 1);
			if(machine->memory->ram[0xFF44] == machine->memory->ram[0xFF45])
				machine->memory->ram[0xFF41] |= (1 << 2);
			else
				machine->memory->ram[0xFF41] &= ~(1 << 2);

			if(machine->memory->ram[0xFF41] & (1 << 5) ||
					(machine->memory->ram[0xFF41] & (1 << 6) &&
					 machine->memory->ram[0xFF44] ==
					 machine->memory->ram[0xFF45]))
				machine->memory->ram[0xFF0F] |= (1 << 1);

			render_scanline(machine);
		}
	}

	if(machine->hblank_mode3_counter <= 0)
	{
		machine->hblank_mode3_counter += HBLANK_PERIOD;
		if(machine->current_scanline < 144)
		{
			machine->memory->ram[0xFF41] |= (1 << 0);
			machine->memory->ram[0xFF41] |= (1 << 1);
		}
	}

	if(machine->hblank_mode0_counter <= 0)
	{
		machine->hblank_mode0_counter += HBLANK_PERIOD;
		if(machine->current_scanline < 144)
		{
			machine->memory->ram[0xFF41] &= ~(1 << 0);
			machine->memory->ram[0xFF41] &= ~(1 << 1);
			if(machine->memory->ram[0xFF41] & (1 << 3))
				machine->memory->ram[0xFF0F] |= (1 << 1);
		}
	}

	if(machine->vblank_counter <= 0)
	{
		vblank_flag = 1;
		machine->vblank_counter += VBLANK_PERIOD;
		machine->memory->ram[0xFF41] |= (1 << 0);
		machine->memory->ram[0xFF41] &= ~(1 << 1);
		if(machine->memory->ram[0xFF41] & (1 << 4))
			machine->memory->ram[0xFF0F] |= (1 << 1);
		machine->memory->ram[0xFF0F] |= (1 << 0);
	}

	if(machine->div_counter <= 0)
	{
		machine->div_counter += DIV_PERIOD;
		machine->memory->ram[0xFF04] += 1;
	}

	int flagsIF = machine->memory->ram[0xFF0F];
	int flagsIE = machine->memory->ram[0xFFFF];
	if(machine->ime && (flagsIF & flagsIE))
	{
		int i = 0;
		while(1)
		{
			if (flagsIF & flagsIE & (1 << i))
				break;
			i++;
		}
		machine->ime = 0;
		machine->memory->ram[0xFF0F] &= ~(1 << i);
		u16 oldPC = machine->cpu->pc;
		u8 hiPC, loPC;
		split_word_16(oldPC, &hiPC, &loPC);
		u16 oldSP = machine->cpu->sp;
		machine->memory->ram[oldSP-1] = hiPC;
		machine->memory->ram[oldSP-2] = loPC;
		machine->cpu->sp -= 2;
		switch(i)
		{
			case 0: machine->cpu->pc = 0x0040; break;
			case 1: machine->cpu->pc = 0x0048; break;
			case 2: machine->cpu->pc = 0x0050; break;
			case 3: machine->cpu->pc = 0x0058; break;
			case 4: machine->cpu->pc = 0x0060; break;
			default: assert(0);
		}
	}

	return vblank_flag;
}

/* returns 1 if a vblank occured
 * otherwise returns 0 */
inline int update_machine(Machine* machine, u8* breakpoint_table)
{
/*
	if(breakpoint_table)
	{
		int bp = query_breakpoint(breakpoint_table, machine->cpu->pc);
		if(bp)
		{
*/

/*
			printf("\
%04X: %02X\n\
af = %02X%02X  z = %d  %04X: %02X\n\
bc = %02X%02X  n = %d  %04X: %02X\n\
de = %02X%02X  h = %d  %04X: %02X\n\
hl = %02X%02X  c = %d  %04X: %02X\n\
sp = %04X  %cI     %04X: %02X\n\
pc = %04X  ROM01  %04X: %02X\n\
                  %04X: %02X\n\
IE:%d%d%d%d%d%d%d%d       %04X: %02X\n\
SCX:%02x SCY:%02x     %04X: %02X\n",
				(int)machine->cpu->pc, (int)read_memory(machine->memory, machine->cpu->pc),
				(int)machine->cpu->a, (int)machine->cpu->f,
				(machine->cpu->f & (1 << 7)) > 0,
				machine->cpu->sp-4,
				read_memory(machine->memory, machine->cpu->sp-4),
				(int)machine->cpu->b, (int)machine->cpu->c,
				(machine->cpu->f & (1 << 6)) > 0,
				machine->cpu->sp-3,
				read_memory(machine->memory, machine->cpu->sp-3),
				(int)machine->cpu->d, (int)machine->cpu->e,
				(machine->cpu->f & (1 << 5)) > 0,
				machine->cpu->sp-2,
				read_memory(machine->memory, machine->cpu->sp-2),
				(int)machine->cpu->h, (int)machine->cpu->l,
				(machine->cpu->f & (1 << 4)) > 0,
				machine->cpu->sp-1,
				read_memory(machine->memory, machine->cpu->sp-1),
				(int)machine->cpu->sp,
				machine->ime ? 'E' : 'D',
				machine->cpu->sp,
				read_memory(machine->memory, machine->cpu->sp),
				(int)machine->cpu->pc,
				machine->cpu->sp+1,
				read_memory(machine->memory, machine->cpu->sp+1),
				machine->cpu->sp+2,
				read_memory(machine->memory, machine->cpu->sp+2),
				(machine->memory->ram[0xFFFF] & (1 << 7)) > 0,
				(machine->memory->ram[0xFFFF] & (1 << 6)) > 0,
				(machine->memory->ram[0xFFFF] & (1 << 5)) > 0,
				(machine->memory->ram[0xFFFF] & (1 << 4)) > 0,
				(machine->memory->ram[0xFFFF] & (1 << 4)) > 0,
				(machine->memory->ram[0xFFFF] & (1 << 2)) > 0,
				(machine->memory->ram[0xFFFF] & (1 << 1)) > 0,
				(machine->memory->ram[0xFFFF] & (1 << 0)) > 0,
				machine->cpu->sp+3,
				read_memory(machine->memory, machine->cpu->sp+3),
				(int)machine->memory->ram[0xFF43],
				(int)machine->memory->ram[0xFF42],
				machine->cpu->sp+4,
				read_memory(machine->memory, machine->cpu->sp+4));
*/


/*
			char c;
			scanf("%c", &c);
		}
	}
*/

	int cycle_count = step_instruction(machine->cpu,
			machine->memory,
			machine,
			instruction_error_handler, 0);
	return irq_update(machine, cycle_count);
}

void update_machine_vblank(Machine* machine, u8* breakpoint_table)
{
	while(update_machine(machine, breakpoint_table) == 0);
}

