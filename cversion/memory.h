#ifndef MEMORY_H__
#define MEMORY_H__

#include "util.h"
#include "rom_image.h"

#define RAM_SIZE 0x10000

typedef struct Memory_
{
	RomImage* rom_image;
	u8* ram;
} Memory;

int init_memory(Memory* memory, RomImage* rom_image);
void free_memory(Memory* memory);

static inline u8 read_memory(Memory* memory, u16 address)
{
	if(address < 0x8000)
		return memory->rom_image->memory[address];
	else
	{
		if(address == 0xFF00)
			return 0x00;
		else
			return memory->ram[address];
	}
}

static inline void write_memory(Memory* memory, u16 address, u8 value)
{
	if(address >= 0x8000)
	{
		if(address >= 0xC000 && address < 0xDE00) /* Echoed Memory */
		{
			memory->ram[address] = value;
			memory->ram[address + 0x2000] = value;
		}
		else if(address >= 0xE000 && address < 0xFE00) /* Echoed Memory */
		{
			memory->ram[address] = value;
			memory->ram[address - 0x2000] = value;
		}
		else if(address == 0xFF04) /* DIV Register */
		{
			memory->ram[0xFF04] = 0x00;
		}
		else if(address == 0xFF07)
		{
			assert(0);
		}
		else if(address == 0xFF44) /* LY Register */
		{
			memory->ram[0xFF04] = 0x00;
		}
		else if(address == 0xFF46) /* DMA Transfer */
		{
			int i;
			for(i = 0xFE00; i <= 0xFE9F; i++)
			{
				memory->ram[i] = read_memory(memory,
						(i-0xFE00) + join_word_16(value, 0));
			}
		}
		else
		{
			memory->ram[address] = value;
		}
	}
}

#endif

