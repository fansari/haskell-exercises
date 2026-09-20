# Makefile for M11 project

PROJ = m11

# The rest of the Makefile uses the variable
TARGET = $(PROJ)
SOURCE = $(PROJ).hs

GHC_FLAGS = -O2 -package containers

# Default target
all: $(TARGET)

# Rule to compile the executable
$(TARGET): $(SOURCE)
	ghc $(GHC_FLAGS) $(SOURCE) -o $(TARGET)
	# ghc -O2 -package containers -package array m11-detect.hs -o m11-detect

# Clean up build artifacts
clean:
	rm -f $(TARGET) *.hi *.o

# Phony targets to ensure commands run even if files exist
.PHONY: all clean
