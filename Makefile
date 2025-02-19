rofi-bookmarks: rofi-bookmarks.lisp
	sbcl --noinform --load rofi-bookmarks.lisp --eval "(sb-ext:save-lisp-and-die \"rofi-bookmarks\" \
							 :toplevel #'main \
							 :executable t \
							 :compression 9)"
