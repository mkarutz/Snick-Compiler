TARGETS = snick
TARGETS_BYTE=$(TARGETS:%=%.byte)

MODULES = ast lexer parser pprinter
MLFILES = $(addsuffix .ml, $(MODULES))
CMOFILES = $(addsuffix .cmo, $(MODULES))
CMXFILES = $(addsuffix .cmx, $(MODULES))

ALLMODULES = $(MODULES) snick

OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLDEP = ocamldep

OCAMLFLAGS =

all : opt byte
byte: $(TARGETS_BYTE)
opt: $(TARGETS)

%.cmi: %.mli
	ocamlc $(OCAMLFLAGS) -c $<

%.cmo: %.ml
	ocamlc $(OCAMLFLAGS) -g -c $<

%.cmx: %.ml
	ocamlopt $(OCAMLOPTFLAGS) -g -c $<

%.ml: %.mll
	$(OCAMLLEX) $^

%.ml %.mli: %.mly
	$(OCAMLYACC) $^

snick.byte : $(CMOFILES) snick.cmo
	ocamlc -g -o $@ $^

snick : $(CMXFILES) snick.cmx
	ocamlopt -g -o $@ $^

clean :
	rm -f *.cmo *.cmi *.cmx *.o
	rm -f lexer.ml parser.ml parser.mli

clobber : clean
	rm -f $(TARGETS) $(TARGETS_BYTE)

.PHONY : clean clobber depend

# include depend
depend: lexer.ml parser.ml
	$(OCAMLDEP) snick.ml snick.mli $(ALLMODULES:%=%.mli) $(ALLMODULES:%=%.ml) >Makefile.depend

test : snick
	python run_tests.py

-include Makefile.depend
