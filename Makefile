# rebar should be in PATH env
REBAR := rebar

PROJECT := epool

.PHONY: all install clean reinstall deps

TMPDIR=~/tmp/

deps:
	$(REBAR) get-deps

all: compile
	$(REBAR) compile

compile:
	$(REBAR) compile

install: all
	$(REBAR) -v generate

clean:
	$(REBAR) clean
	rm -R ebin

reinstall: all
	$(REBAR) -v generate force=1


console:
	./rel/$(PROJECT)/bin/$(PROJECT) console
console_clean:
	./rel/$(PROJECT)/bin/$(PROJECT) console_clean

start:
	./rel/$(PROJECT)/bin/$(PROJECT) start

stop:
	./rel/$(PROJECT)/bin/$(PROJECT) stop

attach:
	./rel/$(PROJECT)/bin/$(PROJECT) attach

