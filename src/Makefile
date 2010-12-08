BUILD = ocamlbuild

FLAGS = \
	-build-dir ../bin

all:
	mkdir -p ../bin
	$(BUILD) $(FLAGS) bouncer.byte

clean:
	$(BUILD) $(FLAGS) -clean

