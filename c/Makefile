CC = gcc
CC_FLAGS = -Wall -O3 -DBOUNDS_CHECK
LN_FLAGS =

INCLUDES = -I.src/

SRC_FILES = $(wildcard src/*.c) $(wildcard src/gen/*.c) $(wildcard src/common/*.c) $(wildcard src/group/*.c)
OBJ_FILES = $(addprefix bin/,$(notdir $(SRC_FILES:.c=.o)))
DEP_FILES = $(OBJ_FILES:.o=.d)

BIN = ./bin/casoc

ifneq "$(findstring prf, $(MAKECMDGOALS))" ""
CC_FLAGS = -Wall -O3 -pg
LN_FLAGS = -pg
endif

ifneq "$(findstring opt, $(MAKECMDGOALS))" ""
CC_FLAGS = -Wall -O3 -DSTRUCT_HACK
LN_FLAGS =
endif

all: $(BIN)
prf: $(BIN)
opt: $(BIN)

$(BIN): $(OBJ_FILES)
	$(CC) $(LN_FLAGS) $^ -o $@

-include $(DEP_FILES)

bin/%.o: src/%.c
	$(CC) $(CC_FLAGS) $(INCLUDES) -MMD -c $< -o $@

bin/%.o: src/gen/%.c
	$(CC) $(CC_FLAGS) $(INCLUDES) -MMD -c $< -o $@

bin/%.o: src/common/%.c
	$(CC) $(CC_FLAGS) $(INCLUDES) -MMD -c $< -o $@

bin/%.o: src/group/%.c
	$(CC) $(CC_FLAGS) $(INCLUDES) -MMD -c $< -o $@

clean:
	-rm $(BIN) $(OBJ_FILES) $(DEP_FILES)
