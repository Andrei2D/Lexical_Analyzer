HDR_DIR = -I./inc
SRC_DIR = $(shell ls ./src)
SRC 	= $(foreach dir, $(SRC_DIR), ./src/$(dir))
EXEC	= LexAutomata

all: run
	

run: build
	./$(EXEC)

build:
	gcc $(SRC) -o $(EXEC) $(HDR_DIR)