.PHONY: build run clean

# Variables
DISPLAY_CMD = DISPLAY=:0
EXEC = _build/default/bin/main.exe

# Compilation
build:
	cd main && dune build

# Exécution
run: build
	cd main && $(DISPLAY_CMD) dune exec $(EXEC)

# Nettoyage
clean:
	cd main && dune clean

# Recompilation complète
rebuild: clean build