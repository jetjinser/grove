<!-- SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu> -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

# Guile SRFI 275

This is a standalone Guile implementation of SRFI 275, *URIs, IRIs, and basic
paths*. Grove is a consumer, not part of the library or its runtime contract.

The public library is split into the five SRFI sublibraries: `iri`, `uri`,
`normalize`, `path`, and `utils`. Guile-native imports use `(srfi :275 iri)`
and the corresponding names for the other four libraries. Guile's R6RS
compatibility mapping resolves each `(srfi :275 name)` import to one aggregate
`(srfi srfi-275)` module; users needing separate module namespaces should use
Guile's native `use-modules` form. This host mapping is documented and tested
as a Guile-specific limitation.

The implementation is ported from the SRFI sample implementation at upstream
commit `33b4a8738fe17d267fb7b3806cb4a6788f60cb75`. The sample is MIT-licensed;
source files retain its copyright and SPDX notices. The conformance tests are
preserved unchanged under `tests/upstream/`; Guile-specific adaptations live
under `tests/guile/`.

The library targets Guile 3.0 and can be used from a checkout by setting
`GUILE_LOAD_PATH=lib`. For example:

```scheme
(use-modules (srfi :275 iri)
             (srfi :275 normalize))

(define address (string->iri "https://例え.テスト/资料"))
(display (iri->string (normalize-iri address)))
```

Run the upstream conformance suite and Guile module-contract checks with
`make check`. The standalone Guix package can be built from this directory
with `guix time-machine -C ../channels.scm -- build -f guix.scm`; it installs
the SRFI modules in Guile's versioned site directory and declares that
directory as a `GUILE_LOAD_PATH` search path. It has no Grove or Blue runtime
dependency.

The port passes the upstream parsing, escaping, setter, path, resolution,
relativization, and normalization tests on Guile 3.0.9 and 3.0.11. Additional
tests verify native module separation, Guile's R6RS aggregate import mapping,
and loading from the installed Guix development profile. Grove consumes the
URI API for Forester entity-URI validation; this integration does not make
Grove part of the library contract.

The port retains the reference implementation's distinction between URI and
IRI, and does not add IDNA or host-name conversion. Guile's R6RS mapper
currently merges SRFI library imports into one aggregate module; native
`use-modules` imports preserve the five separate namespaces.
