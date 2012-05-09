.PHONY: info html doc

doc: info html

info: spice-mode.texinfo
	makeinfo spice-mode.texinfo

html: spice-mode.texinfo
	makeinfo --html spice-mode.texinfo
