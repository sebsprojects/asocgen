BUILDFLAGS = -Wall -O3

INCLUDES = -I./src/

CPP_FILES := $(wildcard src/*.cpp)
OBJ_FILES := $(addprefix bin/,$(notdir $(CPP_FILES:.cpp=.o)))

./bin/casoc: $(OBJ_FILES)
	g++ -o $@ $^

bin/%.o: src/%.cpp
	g++ $(BUILDFLAGS) $(INCLUDES) -c -o $@ $<
