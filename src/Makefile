BUILD = ocamlbuild

FLAGS = \
	-build-dir ../bin

all:
	mkdir -p ../bin
	$(BUILD) $(FLAGS) bouncer.native

clean:
	$(BUILD) $(FLAGS) -clean

