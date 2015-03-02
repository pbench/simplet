
all:
	$(MAKE) -C src all && \
	cp src/_build/main.native simplet

debug: 
	$(MAKE) -C src debug && \
	cp src/_build/main.d.byte debug_simplet


doc:
	$(MAKE) -C src doc && \
	ln -f -s -T  src/_build/main.docdir/index.html doc.html

clean:
	$(MAKE) -C src clean && \
	rm -f ./simplet ./debug_simplet ./doc.html
