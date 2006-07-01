#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

#include "rom_image.h"

int load_rom_image(RomImage* rom, const char* file)
{
	struct stat buf;
	if(stat(file, &buf) == -1)
		return -1;

	rom->size = buf.st_size;
	if(rom->size < 0x8000)
		return -1;

	FILE *fp = fopen(file, "rb");
	if(fp == 0)
		return -1;

	rom->memory = malloc(buf.st_size);
	if(rom->memory == 0)
	{
		fclose(fp);
		return -1;
	}

	if(fread(rom->memory, sizeof(u8), rom->size, fp) < rom->size)
	{
		free(rom->memory);
		fclose(fp);
		return -1;
	}

	fclose(fp);

	printf("ROM loaded\n");
	printf("%s\n", file);
	printf("Title: %.14s\n", &rom->memory[0x0134]);
	printf("Cartridge type: ");
	switch(rom->memory[0x0147])
	{
		case 0x00: puts("ROM ONLY"); break;
		case 0x01: puts("ROM+MBC1"); break;
		case 0x02: puts("ROM+MBC1+RAM"); break;
		case 0x03: puts("ROM+MBC1+RAM+BATT"); break;
		case 0x05: puts("ROM+MBC2"); break;
		case 0x06: puts("ROM+MBC2+BATTERY"); break;
		case 0x08: puts("ROM+RAM"); break;
		case 0x09: puts("ROM+RAM+BATTERY"); break;
		case 0x0B: puts("ROM+MMM01"); break;
		case 0x0C: puts("ROM+MMM01+SRAM"); break;
		case 0x0D: puts("ROM+MMM01+SRAM+BATT"); break;
		case 0x0F: puts("ROM+MBC3+TIMER+BATT"); break;
		case 0x10: puts("ROM+MBC3+TIMER+RAM+BATT"); break;
		case 0x11: puts("ROM+MBC3"); break;
		case 0x12: puts("ROM+MBC3+RAM"); break;
		case 0x13: puts("ROM+MBC3+RAM+BATT"); break;
		case 0x19: puts("ROM+MBC5"); break;
		case 0x1A: puts("ROM+MBC5+RAM"); break;
		case 0x1B: puts("ROM+MBC5+RAM+BATT"); break;
		case 0x1C: puts("ROM+MBC5+RUMBLE"); break;
		case 0x1D: puts("ROM+MBC5+RUMBLE+SRAM"); break;
		case 0x1E: puts("ROM+MBC5+RUMBLE+SRAM+BATT"); break;
		case 0x1F: puts("Pocket Camera"); break;
		case 0xFD: puts("Bandai TAMA5"); break;
		case 0xFE: puts("Hudson HuC-3"); break;
		case 0xFF: puts("Hudson HuC-1"); break;
		default: puts("[* INVALID *]"); break;
	}
	printf("ROM size: ");
	switch(rom->memory[0x0148])
	{
		case 0x00: puts("256Kbit = 32KByte = 2 banks"); break;
		case 0x01: puts("512Kbit = 64KByte = 4 banks"); break;
		case 0x02: puts("1Mbit = 128KByte = 8 banks"); break;
		case 0x03: puts("2Mbit = 256KByte = 16 banks"); break;
		case 0x04: puts("4Mbit = 512KByte = 32 banks"); break;
		case 0x05: puts("8Mbit = 1MByte = 64 banks"); break;
		case 0x06: puts("16Mbit = 2MByte = 128 banks"); break;
		case 0x52: puts("9Mbit = 1.1MByte = 72 banks"); break;
		case 0x53: puts("10Mbit = 1.2MByte = 80 banks"); break;
		case 0x54: puts("12Mbit = 1.5MByte = 96 banks"); break;
		default: puts("[* INVALID *]"); break;
	}
	printf("RAM size: ");
	switch(rom->memory[0x0149])
	{
		case 0x00: puts("None"); break;
		case 0x01: puts("16kBit = 2kB = 1 bank"); break;
		case 0x02: puts("64kBit = 8kB = 1 bank"); break;
		case 0x03: puts("256kBit = 32kB = 4 banks"); break;
		case 0x04: puts("1MBit = 128kB = 16 banks"); break;
		default: puts("[* INVALID *]"); break;
	}
	printf("Destination code: ");
	switch(rom->memory[0x014A])
	{
		case 0x00: puts("Japanese"); break;
		case 0x01: puts("Non-Japanese"); break;
		default: puts("[* INVALID *]"); break;
	}
	printf("Licensee code: ");
	switch(rom->memory[0x0144])
	{
		case 0x79: puts("Accolade"); break;
		case 0xA4: puts("Konami"); break;
		case 0x33: printf("%c%c\n", rom->memory[0x0144], rom->memory[0x0145]);
		default: puts("[* INVALID *]"); break;
	}
	printf("Mask ROM version number: %hhd\n", rom->memory[0x014C]);

	return 0;
}

void free_rom_image(RomImage* rom)
{
	free(rom->memory);
}

