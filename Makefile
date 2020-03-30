clean:
	@rm -f early-init.elc init.elc emacs.el emacs.elc .lsp*

compile: init.el emacs.org clean
	@emacs -Q --batch -l 'lisp/compile.el'

update: 
	@emacs -Q --batch -l "lisp/update.el"
