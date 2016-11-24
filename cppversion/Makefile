BUILDFLAGS = -Wall -O3

INCLUDES = -I./src/

CPP_FILES = $(wildcard src/*.cpp)
OBJ_FILES = $(addprefix bin/,$(notdir $(CPP_FILES:.cpp=.o)))
DEP_FILES = $(OBJ_FILES:.o=.d)

BIN = casoc

./bin/$(BIN): $(OBJ_FILES)
	g++ $^ -o $@

-include $(DEP_FILES)

bin/%.o: src/%.cpp
	g++ $(BUILDFLAGS) $(INCLUDES) -MMD -c $< -o $@

clean:
	-rm ./bin/$(BIN) $(OBJ_FILES) $(DEP_FILES)
