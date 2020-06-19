.PHONY: clean compile
QUIET := @
ERL := erl -I lib/mandala/include -noshell -pa lib/mandala/ebin
ERL_MAKE := erl -make
APP := lib/mandala/ebin/mandala.app
KERNEL := lib/mandala/ebin/Mandala.Kernel.beam

default: compile

compile: erlang $(APP) mandala
	$(QUIET) echo "=> make: compile"

erlang:
	$(QUIET) echo "=> make: erlang"
	$(QUIET) cd lib/mandala && mkdir -p ebin && $(ERL_MAKE)

$(APP): lib/mandala/src/mandala.app.src lib/mandala/ebin
	$(QUIET) echo "=> make: {APP}"

mandala: stdlib
	$(QUIET) echo "=> make: mandala"

stdlib: $(KERNEL)
	$(QUIET) echo "=> make: stdlib"

$(KERNEL): lib/mandala/lib/*.mdl
	$(QUIET) echo "=> make: lib/mandala/lib/*.mdl"
	$(QUIET) echo "==> bootstrap (compile)"
	$(QUIET) $(ERL) -s mandala_compiler bootstrap -s erlang halt
	$(QUIET) echo "==> mandala (compile)"

clean:
	$(QUIET) echo "=> make: clean"
	rm -rf lib/*/ebin