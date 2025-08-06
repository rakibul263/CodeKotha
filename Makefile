CXX = g++
CXXFLAGS = -std=c++17 -Wall -Wextra -O2
TARGET = banglang_compact
SOURCE = banglang_compact.cpp

all: $(TARGET)

$(TARGET): $(SOURCE)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(SOURCE)

clean:
	rm -f $(TARGET) output.txt output_c.txt

# Default target: compile input.txt to output.txt
run: $(TARGET)
	./$(TARGET)

# Generate 3-address code
3ac: $(TARGET)
	./$(TARGET) -o output.txt

# Generate C code
c: $(TARGET)
	./$(TARGET) -c -o output_c.txt

# Test with different input files
test: $(TARGET)
	./$(TARGET) -i simple.bang -o simple_output.txt
	./$(TARGET) -i sample.bang -o sample_output.txt
	./$(TARGET) -i input.txt -o output.txt

.PHONY: all clean run 3ac c test 