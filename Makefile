CC       ?= clang
CXX      ?= clang++
CFLAGS   := -Wall -Wextra -Werror -std=c11 `llvm-config --cflags`
LD_FLAGS := `llvm-config --cxxflags --ldflags --libs engine --system-libs`
INC      = -Isrc

BUILD_DIR ?= build
INT_DIR   := $(BUILD_DIR)/int
SRC_DIR   =  src

SRCS := $(shell find $(SRC_DIR) -name "*.c")
HEADERS := $(shell find $(SRC_DIR) -name "*.h")
DEBUG_OBJS := $(patsubst $(SRC_DIR)/%.c, $(INT_DIR)/debug/%.o, $(SRCS))
RELEASE_OBJS := $(patsubst $(SRC_DIR)/%.c, $(INT_DIR)/release/%.o, $(SRCS))

.PHONY: debug release all test clean 

debug: $(BUILD_DIR)/sicdb

release: $(BUILD_DIR)/sic

all: debug release

test: debug
	python3 tests/run_tests.py

clean:
	rm -rf $(BUILD_DIR)

$(BUILD_DIR)/sicdb: $(DEBUG_OBJS)
	@echo 'Linking sicdb (debug)'
	@$(CXX) $(INC) $(LD_FLAGS) -ggdb -o $@ $^

$(BUILD_DIR)/sic: $(RELEASE_OBJS)
	@echo 'Linking sic (release)'
	@$(CXX) $(INC) $(LD_FLAGS) -O3 -o $@ $^

$(DEBUG_OBJS): $(INT_DIR)/debug/%.o: $(SRC_DIR)/%.c $(HEADERS)
	@mkdir -p $(dir $@)
	@echo 'Making $@ (debug)'
	@$(CC) $(CFLAGS) $(EXTRACFLAGS) $(INC) -DSI_DEBUG -ggdb -o $@ -c $<

$(RELEASE_OBJS): $(INT_DIR)/release/%.o: $(SRC_DIR)/%.c $(HEADERS)
	@mkdir -p $(dir $@)
	@echo 'Making $@ (release)'
	@$(CC) $(CFLAGS) $(EXTRACFLAGS) $(INC) -O3 -o $@ -c $<
