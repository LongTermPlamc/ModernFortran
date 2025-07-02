FC = caf

# declare respective directories.
CHAPTER_DIR := .
## directory with main app
APP_DIR := $(CHAPTER_DIR)/app
## directory with lib files
LIB_DIR := $(CHAPTER_DIR)/../../modernLib/src
## directory with libs
BUILD_DIR := $(CHAPTER_DIR)/build

$(shell mkdir -p $(BUILD_DIR))

# source files
## source for library
LIB_SRC := $(wildcard $(LIB_DIR)/*.f90)
## source for app
APP_SRC := $(wildcard $(APP_DIR)/tsunami.f90)

# object files
LIB_OBJ := $(patsubst $(LIB_DIR)/%.f90, $(BUILD_DIR)/lib_%.o, $(LIB_SRC))
APP_OBJ := $(patsubst $(APP_DIR)/%.f90, $(BUILD_DIR)/app_%.o, $(APP_SRC))

$(info [LIB_SRC] = $(LIB_SRC))
$(info [LIB_OBJ] = $(LIB_OBJ))
$(info [APP_SRC] = $(APP_SRC))
$(info [APP_OBJ] = $(APP_OBJ))

# executable
EXE := main.exe

# compilation rules

## Default
### Target: dependencies
all: $(EXE)

### Linking rules
$(EXE): $(LIB_OBJ) $(APP_OBJ)
	$(FC) -o $@ $^

$(BUILD_DIR)/lib_%.o: $(LIB_DIR)/%.f90
	$(FC) -c -J $(BUILD_DIR) -o $@ $<

$(BUILD_DIR)/app_%.o: $(APP_DIR)/%.f90
	$(FC) -c -I $(BUILD_DIR) -J $(BUILD_DIR) -o $@ $<

clean:
	rm -rf $(BUILD_DIR) *.exe *.mod

.PHONY: all clean