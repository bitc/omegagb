#ifndef ROM_IMAGE_H__
#define ROM_IMAGE_H__

#include "util.h"

typedef struct
{
	int size;
	u8* memory;
} RomImage;

int load_rom_image(RomImage* rom, const char* file);

void free_rom_image(RomImage* rom);

#endif

