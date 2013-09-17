.PHONY: always clean all

EFILES:=markup
PACKAGES:=parsec html xml
HSFLAGS:=$(addprefix -package ,$(PACKAGES))
HSFLAGS+= -fwarn-incomplete-patterns

all: $(EFILES)

$(EFILES): %: always
	ghc $(HSFLAGS) --make $@

clean:
	find . -regex '.*\.\(hi\|o\)$$' -delete
	rm -f $(EFILES)
