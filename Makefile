all: build build/main

build:
	mkdir -p build

build/main: src/main.idr
	idris -p sdl -p effects src/main.idr -o build/main

clean:
	$(RM) -r build
