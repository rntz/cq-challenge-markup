.PHONY: always clean all

EFILES:=markup
PACKAGES:=parsec html xml
HSFLAGS:=$(addprefix -package ,$(PACKAGES))

all: $(EFILES)

$(EFILES): %: always
	ghc $(HSFLAGS) --make $@

clean:
	find . -regex '.*\.\(hi\|o\)$$' -delete
	rm -f $(EFILES)
