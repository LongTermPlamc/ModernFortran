# Compiler and Flags
FC = gfortran
FFLAGS = -Wall -O2 -fcoarray=lib
LIBDIR = /usr/lib/x86_64-linux-gnu/open-coarrays/openmpi/lib
LIBS = -lcaf_mpi

# Directories
SRC_DIR = src
APP_DIR = app
BUILD_DIR = build/makeFiles

# Source Files
MODULE_SOURCES = $(wildcard $(SRC_DIR)/*.f90)
MAIN_SRC = $(APP_DIR)/main.f90
HELLO_SRC = $(APP_DIR)/hello.f90
COARR_SRC = $(APP_DIR)/coarrayAndParallel.f90

# Object Files
MODULE_OBJS = $(patsubst $(SRC_DIR)/%.f90,$(BUILD_DIR)/%.o,$(MODULE_SOURCES))
MAIN_OBJ = $(BUILD_DIR)/main.o
HELLO_OBJ = $(BUILD_DIR)/hello.o
COARR_OBJ = $(BUILD_DIR)/coarrayAndParallel.o

# Executables
MAIN_EXE = main
HELLO_EXE = hello
COARR_EXE = coarrayAndParallel

# Module path flags
MOD_FLAGS = -I$(BUILD_DIR) -J$(BUILD_DIR)

.PHONY: all clean run-main run-hello

# Default target
all: $(MAIN_EXE) $(HELLO_EXE) $(COARR_EXE)

# Ensure build directory exists
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Compile modules
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) $(MOD_FLAGS) -c $< -o $@

# Compile main program
$(MAIN_OBJ): $(MAIN_SRC) | $(BUILD_DIR)
	$(FC) $(FFLAGS) $(MOD_FLAGS) -c $< -o $@

# Compile hello program
$(HELLO_OBJ): $(HELLO_SRC) | $(BUILD_DIR)
	$(FC) $(FFLAGS) $(MOD_FLAGS) -c $< -o $@

# Compile coarrayAndParallel program
$(COARR_OBJ): $(COARR_SRC) | $(BUILD_DIR)
	$(FC) $(FFLAGS) $(MOD_FLAGS) -c $< -o $@

# Link main
$(MAIN_EXE): $(MODULE_OBJS) $(MAIN_OBJ)
	$(FC) $(FFLAGS) $(MOD_FLAGS) -L$(LIBDIR) -o $@ $^ $(LIBS)

# Link hello
$(HELLO_EXE): $(MODULE_OBJS) $(HELLO_OBJ)
	$(FC) $(FFLAGS) $(MOD_FLAGS) -L$(LIBDIR) -o $@ $^ $(LIBS)

# Link coarrayAndParallel program
$(COARR_EXE): $(MODULE_OBJS) $(COARR_OBJ)
	$(FC) $(FFLAGS) $(MOD_FLAGS) -L$(LIBDIR) -o $@ $^ $(LIBS)

# Run targets
run-main: $(MAIN_EXE)
	cafrun -n 4 ./$(MAIN_EXE)

run-hello: $(HELLO_EXE)
	cafrun -n 4 ./$(HELLO_EXE)

run-coarr: $(COARR_EXE)
	cafrun -n 4 ./$(COARR_EXE)

# Cleanup
clean:
	rm -rf $(MAIN_EXE) $(HELLO_EXE) $(COARR_EXE) $(BUILD_DIR)
