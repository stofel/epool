##
## Include in src/Makefile
##


# Release project name
PROJECT := epool

# Erlang compiler
ERLC := erlc
# Includes
I := -I$(ROOT)deps -I$(ROOT)include
# Release number
REL := 1

## Some make config
BEAMS = $(SOURCES:.erl=.beam)
.PHONY: all $(SOURCES) $(BEAMS)


#############################################################
#############################################################
# CMDs
all: $(BEAMS) clean
	#done

$(BEAMS): $(SOURCES)
	cp $@ $(ROOT)rel/$(PROJECT)/lib/$(PROJECT)-$(REL)/ebin

$(SOURCES):
	$(ERLC) $(I) $@

clean:
	rm *.beam

