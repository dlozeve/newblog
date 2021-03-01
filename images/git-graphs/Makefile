INPUTS=$(wildcard *.m4)
TARGETS=$(INPUTS:.m4=.svg)

.PHONY: all
all: $(TARGETS)

%.svg: %.m4 master.dot merged.dot feature.dot bugfix.dot
	m4 $< | dot -Tsvg -o $@

.PHONY: clean
clean:
	rm -f $(TARGETS)
