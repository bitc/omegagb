#include <stdlib.h>
#include <string.h>

#include "memory.h"

int init_memory(Memory* memory, RomImage* rom_image)
{
	memory->rom_image = rom_image;
	memory->ram = malloc(RAM_SIZE);

	if(memory->ram == 0)
		return -1;

	memset(memory->ram, 0, RAM_SIZE);

	return 0;
}

void free_memory(Memory* memory)
{
	free(memory->ram);
}

