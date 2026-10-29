# SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
# SPDX-License-Identifier: GPL-3.0-or-later

GUILE ?= guile
PREFIX ?= /usr/local
GUILE_SITE = $(PREFIX)/share/guile/site/$(shell $(GUILE) -c '(display (effective-version))')

.PHONY: check install

check:
	GUILE_AUTO_COMPILE=0 GUILE_LOAD_PATH="$(CURDIR)/lib" $(GUILE) --no-auto-compile tests/run.scm
	GUILE_AUTO_COMPILE=0 GUILE_LOAD_PATH="$(CURDIR)/lib" $(GUILE) --no-auto-compile tests/module-contract.scm
	GUILE_AUTO_COMPILE=0 GUILE_LOAD_PATH="$(CURDIR)/lib" $(GUILE) --no-auto-compile --r6rs tests/r6rs-import.scm
	GUILE_AUTO_COMPILE=0 GUILE_LOAD_PATH="$(CURDIR)/lib" $(GUILE) --no-auto-compile tests/api-semantics.scm

install:
	install -d "$(DESTDIR)$(GUILE_SITE)/srfi"
	cp -R lib/srfi/. "$(DESTDIR)$(GUILE_SITE)/srfi/"
