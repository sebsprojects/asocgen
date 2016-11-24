CC = g++
CC_FLAGS = -Wall -O3
LN_FLAGS =

INCLUDES = -I./src/

CPP_FILES = $(wildcard src/*.cpp)
OBJ_FILES = $(addprefix bin/,$(notdir $(CPP_FILES:.cpp=.o)))
DEP_FILES = $(OBJ_FILES:.o=.d)

BIN = ./bin/casoc

ifneq "$(findstring prf, $(MAKECMDGOALS))" ""
CC_FLAGS = -Wall -O3 -pg
LN_FLAGS = -pg
endif

ifneq "$(findstring opt, $(MAKECMDGOALS))" ""
CC_FLAGS = -Wall -O3
LN_FLAGS =
endif


all: $(BIN)
prf: $(BIN)
opt: $(BIN)

$(BIN): $(OBJ_FILES)
	$(CC) $(LN_FLAGS) $^ -o $@

-include $(DEP_FILES)

bin/%.o: src/%.cpp
	$(CC) $(CC_FLAGS) $(INCLUDES) -MMD -c $< -o $@

clean:
	-rm $(BIN) $(OBJ_FILES) $(DEP_FILES)
