BUILD = ocamlbuild

FLAGS = \
	-build-dir ../bin

all:
	mkdir -p ../bin
	$(BUILD) $(FLAGS) bouncer.byte
	ln -s ../bin/bouncer.byte ../bin/bouncer
	$(BUILD) $(FLAGS) checkParser.byte
	ln -s ../bin/checkParser.byte ../bin/checkParser

clean:
	$(BUILD) $(FLAGS) -clean

