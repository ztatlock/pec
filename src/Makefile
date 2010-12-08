BUILD = ocamlbuild

FLAGS = \
	-build-dir ../bin

all:
	mkdir -p ../bin
	$(BUILD) $(FLAGS) bouncer.byte
	$(BUILD) $(FLAGS) checkParser.byte

clean:
	$(BUILD) $(FLAGS) -clean

