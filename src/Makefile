BUILD = ocamlbuild
SLINK = ln --symbolic --force

FLAGS = \
	-cflag -g \
	-lflag -g \
	-build-dir ../bin \

all:
	mkdir -p ../bin
	$(BUILD) $(FLAGS) bouncer.byte
	$(SLINK) ../bin/bouncer.byte ../bin/bouncer
	$(BUILD) $(FLAGS) ckparse.byte
	$(SLINK) ../bin/ckparse.byte ../bin/ckparse

clean:
	$(BUILD) $(FLAGS) -clean

