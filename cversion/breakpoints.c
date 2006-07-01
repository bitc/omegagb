#include <string.h>

#include "breakpoints.h"

void clear_all_breakpoints(u8* breakpoint_table)
{
	memset(breakpoint_table, 0, BREAKPOINT_TABLE_SIZE);
}

void set_breakpoint(u8* breakpoint_table, u16 pc)
{
	breakpoint_table[pc / 8] |= 1 << (pc % 8);
}

void clear_breakpoint(u8* breakpoint_table, u16 pc)
{
	breakpoint_table[pc / 8] &= ~(1 << (pc % 8));
}

