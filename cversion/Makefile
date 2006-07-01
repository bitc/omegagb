# C compiler is traditionally called CC
CC = gcc

# The traditional name for the flags to pass to the preprocessor.
# Useful if you need to specify additional directories to look for
# header files.
CPPFLAGS = `pkg-config --cflags gtk+-2.0`

# Another traditional name for the C compiler flags
#CFLAGS = -g -Wall `pkg-config --cflags gtk+-2.0`
#CFLAGS = -O3 -Wall `pkg-config --cflags gtk+-2.0`
CFLAGS = -O -Wall `pkg-config --cflags gtk+-2.0`
#CFLAGS = -O3 -fomit-frame-pointer -pipe -Wall `pkg-config --cflags gtk+-2.0`

# Command to make an object file:
COMPILE = $(CC) $(CPPFLAGS) $(CFLAGS) -c

# Command used to link objects:
# LD is sort of traditional as well,
# probably stands for Linker-loaDer
LD = cc

# The flags to pass to the linker
# At UW CSE, if you are using /uns/g++ as your compiler,
# You'll probably want to add -Wl,-rpath,/uns/lib here.
# This way you won't have to set the LD_LIBRARY_PATH environment
# variable just to run the programs you compile.
LDFLAGS = # -Wl,-rpath,/uns/lib

# Any libraries we may want to use
# you would need a -lm here if you were using math functions.
LIBS = `pkg-config --libs gtk+-2.0`

# The name of the executable we are trying to build
PROGRAM = main

# The object files we need to build the program
OBJS = breakpoints.o cpu.o main.o machine.o memory.o rom_image.o

$(PROGRAM) : $(OBJS)
	$(LD) $(LDFLAGS) $^ $(LIBS) -o $@

-include $(OBJS:.o=.d)

#%.d : %.c
#	set -e; $(CC) -MM $(CPPFLAGS) $< \
#		| sed 's/\($*\)\.o[ :]*/\1.o $@ : /g' > $@; \
#		[ -s $@ ] || rm -f $@

%.d: %.c
	$(CC) -MM $(CPPFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

%.o : %.c
	$(COMPILE) $< -o $@

clean :
	rm -f main $(OBJS)
