BUILD = ocamlbuild
SLINK = ln --symbolic --force

FLAGS = \
	-build-dir ../bin

all:
	mkdir -p ../bin
	$(BUILD) $(FLAGS) bouncer.byte
	$(SLINK) ../bin/bouncer.byte ../bin/bouncer
	$(BUILD) $(FLAGS) checkParser.byte
	$(SLINK) ../bin/checkParser.byte ../bin/checkParser

clean:
	$(BUILD) $(FLAGS) -clean

