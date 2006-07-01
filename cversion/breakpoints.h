#ifndef BREAKPOINTS_H__
#define BREAKPOINTS_H__

#include "util.h"

#define BREAKPOINT_TABLE_SIZE (0x10000 / 8)

void clear_all_breakpoints(u8* breakpoint_table);

void set_breakpoint(u8* breakpoint_table, u16 pc);
void clear_breakpoint(u8* breakpoint_table, u16 pc);

inline static int query_breakpoint(u8* breakpoint_table, u16 pc)
{
	return (breakpoint_table[pc / 8] & (1 << (pc % 8))) > 0;
}

#endif

