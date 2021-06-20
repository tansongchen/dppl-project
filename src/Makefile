# 
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable file f
#   make windows to rebuild the executable file f.exe
#   make test    to rebuild the executable and run it on input file test.f
#   make clean   to remove all intermediate and temporary files
#   make depend  to rebuild the intermodule dependency graph that is used
#                  by make to determine which order to schedule 
#	           compilations.  You should not need to do this unless
#                  you add new modules or new dependencies between 
#                  existing modules.  (The graph is stored in the file
#                  .depend)

# These are the object files needed to rebuild the main executable file
#
OBJS = support.cmo syntax.cmo core.cmo parser.cmo lexer.cmo main.cmo

# Files that need to be generated from other files
DEPEND += lexer.ml parser.ml 

# When "make" is invoked with no arguments, we build an executable 
# typechecker, after building everything that it depends on
all: $(DEPEND) $(OBJS) f

# Include an automatically generated list of dependencies between source files
include .depend

# Build an executable typechecker
f: $(OBJS) main.cmo 
	@echo Linking $@
	ocamlfind ocamlc -package mpi -linkpkg -o $@ $(COMMONOBJS) $(OBJS) 

# Build and test
test: all
	mpiexec -n 5 ./f test.f

# Compile an ML module interface
%.cmi : %.mli
	ocamlfind ocamlc -package mpi -c $<

# Compile an ML module implementation
%.cmo : %.ml
	ocamlfind ocamlc -package mpi -c $<

# Generate ML files from a parser definition file
parser.ml parser.mli: parser.mly
	@rm -f parser.ml parser.mli
	ocamlyacc -v parser.mly
	@chmod -w parser.ml parser.mli

# Generate ML files from a lexer definition file
%.ml %.mli: %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@

# Clean up the directory
clean::
	rm -rf lexer.ml parser.ml parser.mli *.o *.cmo *.cmi parser.output \
	   f f.exe TAGS *~ *.bak

# Rebuild intermodule dependencies
depend:: $(DEPEND) 
	ocamldep $(INCLUDE) *.mli *.ml > .depend

# 
