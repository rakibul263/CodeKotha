#!/bin/bash

echo "BangLang Compiler - Simple Usage"
echo "================================"

# Check if input.txt exists
if [ ! -f "input.txt" ]; then
    echo "❌ Error: input.txt file not found!"
    echo "Please create input.txt with your BangLang code."
    exit 1
fi

# Compile
echo "🔨 Compiling input.txt -> output.txt"
./banglang_compact

if [ $? -eq 0 ]; then
    echo "✅ Success! Check output.txt for results."
    echo ""
    echo "📄 Output preview:"
    echo "=================="
    head -10 output.txt
else
    echo "❌ Compilation failed!"
    exit 1
fi 