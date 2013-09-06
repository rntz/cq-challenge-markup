.PHONY: always clean all

EFILES=markup runtest normalize
all: $(EFILES)

$(EFILES): %: always
	ghc -package parsec -package xml --make $@

clean:
	find . -regex '.*\.\(hi\|o\)$$' -delete
	rm -f $(EFILES)
