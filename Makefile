TARGET	:= $(HOME)/.emacs.d
EMACS	:= emacs

.PHONY: all install clean clean_elpa

all: clean install

install_npm: node_modules/.bin/jslint
	@-npm install

install: clean install_npm
	ln -sf $(CURDIR) $(TARGET)

clean:
	rm -rf $(TARGET)

clean_elpa:
	git clean -qfxd
