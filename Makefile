.PHONY: info html doc

doc: info html spice-mode.pdf

info: spice-mode.texinfo
	makeinfo spice-mode.texinfo

html: spice-mode.texinfo
	makeinfo --html spice-mode.texinfo

spice-mode.pdf: spice-mode.texinfo
	mkdir -p out
	cd out && texi2dvi -q -b ../spice-mode.texinfo
	dvipdf out/spice-mode.dvi spice-mode.pdf && rm -rf out
