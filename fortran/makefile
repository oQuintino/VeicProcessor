FC=gfortran
SRC=emiss.f90
BIN=build/emiss.exe

FLAGS=-w -g0 -O2 -mtune=native

.PHONY: all clean run

ifeq (, $(shell which gfortran))
	$(error ❌ gfortran not found)
endif

all: $(BIN)

$(BIN): $(SRC)
	@mkdir -p build
	@$(FC) $(FLAGS) $< -o $@ 2> /dev/null || (echo "❌ Failed to compile $<"; exit 1)

run: $(BIN)
	@bash run.sh

clean:
	rm -rf build
