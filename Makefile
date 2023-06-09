CC = gcc
CC_FLAGS = -Wall -O3 -std=c99 -I../elfclib/include
LN_FLAGS = -lm -L../elfclib/bin -lelfc

# ----------------------------------------------------------------------------

# Testing
ifneq "$(findstring test, $(MAKECMDGOALS))" ""
SRC_FILES = $(wildcard test/test.c)
else
SRC_FILES = $(wildcard src/*.c)
endif

ifeq ($(BOUNDS_CHECK), 1)
CC_FLAGS += -DBOUNDS_CHECK
endif

VERSION = undefined
CC_FLAGS += -DVERSION='"$(VERSION)"'

# ----------------------------------------------------------------------------

INCLUDES = -I src/

SRC_FILES += $(wildcard src/group/*.c) \
	     $(wildcard src/application/*.c)
OBJ_FILES = $(addprefix bin/,$(notdir $(SRC_FILES:.c=.o)))
DEP_FILES = $(OBJ_FILES:.o=.d)

BIN = ./bin/asoc
TEST = ./bin/testasoc

all: $(BIN)
test: $(TEST)

$(BIN): $(OBJ_FILES)
	$(CC) $^ $(LN_FLAGS) -o $@

$(TEST): $(OBJ_FILES)
	$(CC) $^ $(LN_FLAGS) -o $@

-include $(DEP_FILES)

# ----------------------------------------------------------------------------

bin/%.o: test/%.c
	$(CC) $(CC_FLAGS) $(INCLUDES) -MMD -c $< -o $@

bin/%.o: src/%.c
	$(CC) $(CC_FLAGS) $(INCLUDES) -MMD -c $< -o $@

bin/%.o: src/group/%.c
	$(CC) $(CC_FLAGS) $(INCLUDES) -MMD -c $< -o $@

bin/%.o: src/application/%.c
	$(CC) $(CC_FLAGS) $(INCLUDES) -MMD -c $< -o $@

clean:
	-rm ./bin/*
