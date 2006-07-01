#ifndef UTIL_H_
#define UTIL_H_

#include <glib.h>

typedef guint8 u8;
typedef gint8 s8;

typedef guint16 u16;
typedef gint16 s16;

static inline u16 join_word_16(u8 hi, u8 lo)
{
	return ((u16)(lo)) + (((u16)(hi)) << 8);
}

static inline void split_word_16(u16 nn, u8* hi, u8* lo)
{
	*hi = (u8)(nn >> 8);
	*lo = (u8)(nn);
}

#endif

