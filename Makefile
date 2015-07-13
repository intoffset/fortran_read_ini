# Makefile

.PHONY: all clean

.SUFFIXES:
.SUFFIXES: .o .f

FC       =gfortran
FFLAGS   =-Wall
TARGET   =main
OBJS     =read_utility.o main.o
HEADERS  =
INCLUDES =
LDFLAGS  =
LIBS     =

all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) -o $@ $(FFLAGS) $^ $(INCLUDES) $(LDFLAGS) $(LIBS)

%.o: %.f
	$(FC) -o $@ $(FFLAGS) -c $< $(INCLUDES)


clean:
	rm -f $(TARGET) $(OBJS) *.mod
