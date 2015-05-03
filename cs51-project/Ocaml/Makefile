all: ps6

FILES=expandkey.ml
ps6: $(FILES)
	@echo "Compiling..."
	corebuild expandkey.native 

check: $(FILES)
	chmod u+x ../check_width
	../check_width expandkey.ml

clean:
	rm -rf _build *.native
