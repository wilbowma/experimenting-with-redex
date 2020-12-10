all: htmls

%.html: %.scrbl
	scribble --html --redirect-main "https://docs.racket-lang.org" ++xref-in setup/xref load-collections-xref $<

htmls: *.scrbl *.css
	scribble --htmls --redirect-main "https://docs.racket-lang.org" ++xref-in setup/xref load-collections-xref experimenting.scrbl

html: *.scrbl *.css
	scribble --html --redirect-main "https://docs.racket-lang.org" ++xref-in setup/xref load-collections-xref experimenting.scrbl

pdf: *.scrbl *.tex
	scribble --pdf  --redirect-main "https://docs.racket-lang.org" ++xref-in setup/xref load-collections-xref experimenting.scrbl

sync:
	truncate -s 0 experimenting/*.js
	rsync -avz --delete-excluded --delete experimenting/* http@wjb:www/doc/experimenting-with-redex/
