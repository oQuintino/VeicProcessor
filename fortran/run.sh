#!/bin/bash
set -e

# Absolute path to the project root
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_ROOT/data/raw" || exit 1

if [ ! -f "$PROJECT_ROOT/fortran/build/emiss.exe" ]; then
    echo "❌ Executable not found. Run 'make' first."
    exit 1
fi

# Runs emiss.exe from the build folder, but while being in data/raw
"$PROJECT_ROOT/fortran/build/emiss.exe"

cd - > /dev/null
