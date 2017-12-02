CC = gcc

INCLUDES = -I src/

SRC_FILES = $(wildcard src/*.c) \
	    $(wildcard src/gen/*.c) \
	    $(wildcard src/common/*.c) \
	    $(wildcard src/group/*.c) \
	    $(wildcard src/ring/*.c)
OBJ_FILES = $(addprefix bin/,$(notdir $(SRC_FILES:.c=.o)))
DEP_FILES = $(OBJ_FILES:.o=.d)

BIN = ./bin/casoc

CC_FLAGS = -Wall -O3 -DBOUNDS_CHECK
LN_FLAGS = -lm

ifneq "$(findstring prf, $(MAKECMDGOALS))" ""
CC_FLAGS = -Wall -O3 -pg -no-pie
LN_FLAGS = -pg -lm -no-pie
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

bin/%.o: src/ring/%.c
	$(CC) $(CC_FLAGS) $(INCLUDES) -MMD -c $< -o $@

clean:
	-rm $(BIN) $(OBJ_FILES) $(DEP_FILES)
