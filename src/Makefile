BUILD = ocamlbuild
SLINK = ln --symbolic --force

FLAGS = \
	-cflag -g \
	-lflag -g \
	-build-dir ../bin \

all:
	mkdir -p ../bin
	$(BUILD) $(FLAGS) pec.byte
	$(BUILD) $(FLAGS) ckparse.byte
	$(SLINK) ../bin/pec.byte ../bin/pec
	$(SLINK) ../bin/ckparse.byte ../bin/ckparse

clean:
	$(BUILD) $(FLAGS) -clean

