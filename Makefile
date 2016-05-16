SCHEME_INTERPRETER=csi -I .. -q -s

all: build-all-tests clean

build-all-tests: $(patsubst test/%.scm, test/%.output, $(wildcard test/*.scm))

test/%.output: test/%.scm Makefile
	$(SCHEME_INTERPRETER) $(SCHEME_INTERPRETER_FLAGS) $<

clean:
	rm -f *.output

