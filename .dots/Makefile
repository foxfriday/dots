SELF_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
CONFIG_DIR := $(HOME)/.config/emacs

.PHONY: help
help:		## Show this help.
	$(info Commands:)
	@sed -e '/#\{2\}/!d; s/\\$$//; s/:[^#\t]*/:/; s/#\{2\} *//' $(MAKEFILE_LIST)

.PHONY: init
init:		## Initialize repository
	git init --bare "${HOME}/.dots.git"
	git --git-dir="${HOME}/.dots.git/" --work-tree="${HOME}" config --local status.showUntrackedFiles no

.PHONY: system
system:		## Activate additional system services
	systemctl --user enable --now blueman.service
	systemctl --user enable --now dropbox.service
	systemctl --user enable --now kanshi.service
	systemctl --user enable --now rclone.timer

.PHONY: emacsd
emacsd:		## Compile Emacs' configuration files
	emacs -batch --eval '(package-initialize)' -f batch-byte-compile ${HOME}/.config/emacs/lisp/*.el

.PHONY: clean
clean:		## Remove compiled Emacs' configuration files
	rm ${HOME}/.config/emacs/lisp/*.elc
