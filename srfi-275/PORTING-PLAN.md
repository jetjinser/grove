<!-- SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu> -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

# Guile SRFI 275 implementation plan

This directory is the root of a standalone Guile implementation of SRFI 275.
Grove is its first consumer, but the implementation must not depend on Grove.
The public contract is the five SRFI libraries: `iri`, `uri`, `normalize`,
`path`, and `utils`. A Guile-idiomatic module entry point may be added where
Guile's R6RS library-name mapping requires it; such an adapter must expose the
same standard procedures and must not fork their semantics.

The implementation is based on upstream SRFI 275 commit
`33b4a8738fe17d267fb7b3806cb4a6788f60cb75` (2026-10-15). The upstream sample
is MIT-licensed. Preserve its copyright and license notices in derived source.
Do not copy unrelated website assets or their licenses into this library.

## Completed work

1. **Package and module contract — complete**
   - Added the five Guile-native public modules and the aggregate R6RS module.
   - Documented and tested Guile's R6RS mapping behavior.
   - Added an independent Guix package with a declared Guile site search path;
     it builds without Grove, Blue, or Flora as a dependency.

2. **Portable implementation substrate — complete**
   - Port shared resource-identifier records, component encoding, character
     sets, UTF-8 handling, and path segment representation.
   - Replace Chez record printing/equality and formatted assertion helpers
     with Guile-compatible behavior while preserving SRFI contracts.
   - Replace or adapt SRFI 160/152/262 facilities only where Guile lacks the
     required operations. Keep encoded octets distinct from Unicode text.

3. **Paths and utilities — complete**
   - Implement the complete public `path` and `utils` APIs.
   - Port their upstream tests and add Guile-specific import/install tests.

4. **URI and IRI libraries — complete**
   - Port both parsers, accessors, persistent and mutable setters, string
     readers/writers, predicates, and URI/IRI conversion.
   - Keep URI and IRI records distinct as specified. Do not introduce IDNA
     behavior into generic SRFI conversion unless the standard requires it.
   - Run the upstream parsing, escaping, setter, and relative-reference tests.

5. **Normalization and resolution — complete**
   - Port normalization, equivalence, merge/resolve, and relativization
     procedures, preserving the SRFI's specified behavior for ambiguous path
     normalization cases.
   - Port the complete normalization and relative-reference test groups.

6. **Conformance and packaging — complete**
   - Run all portable upstream tests under Guile; retain assertions and expected
     results, adapting only the Chez test harness and explicitly internal tests.
   - Preserved all upstream suites and their expected failures; added Guile
     module-contract and R6RS import tests. Unicode and malformed UTF-8 cases
     are covered by the upstream assertions.
   - Ported tests adapt Chez match and list-quantifier syntax. Character-set
     property tests use representative ASCII, BMP, and supplementary-plane
     samples because enumerating the full Unicode complement is impractical.
     The runner accumulates unexpected failures across SRFI-64 suites.
   - Verified `make check` in the Guix build environment and built the package
     with the repository's pinned channel.

7. **Grove consumer validation — complete**
   - Use the standalone package from Grove for IRI validation and URI
     conversion at the appropriate boundaries.
   - Keep local filesystem paths, routes, and resource identity semantically
     distinct.
   - Switched Forester entity-URI parsing to the standalone URI and path
     modules, added focused consumer tests, and ran the full default-parallel
     `blue check` successfully.

## Porting rules

- Treat the upstream SRFI text and tests as normative; the Chez sample is a
  starting implementation, not a substitute for the specification.
- Keep implementation-specific records private. Consumers depend on the
  public procedures and value semantics, not a particular in-memory layout.
- Expose the standard Guile module paths `(srfi :275 iri)`, `(srfi :275 uri)`,
  `(srfi :275 normalize)`, `(srfi :275 path)`, and `(srfi :275 utils)`, plus
  the aggregate `(srfi srfi-275)` needed by Guile's R6RS library-name mapping.
- Do not silently weaken validation or change comparison/normalization
  behavior to match Grove's current string handling.
- Keep the full reference test suite as a conformance asset and record every
  intentional host-specific adaptation.
- Commit completed, independently reviewable milestones with title-only
  commit messages.

## Outcomes and remaining boundaries

The implementation is a standalone Guile 3.0 package with all five public
sublibraries. The upstream sample's representation is retained behind the
public API, with Guile SRFI-4 vectors and Guile-compatible module/matcher
behavior. During validation, two reference-sample defects were corrected:
`path=?` now distinguishes absolute and relative paths, and the relative-path
constructor no longer duplicates its first segment when disambiguating a
colon-leading path. URI and IRI serialization also preserve the difference
between an absent authority and an explicitly empty authority, and
`resolve-uri` returns a URI.

IDNA, web fetch behavior, and host-specific R6RS namespace separation remain
outside this SRFI port. IDNA is deliberately not implicit in generic URI/IRI
parsing; Guile's R6RS importer limitation is documented, with native imports
provided for namespace isolation.
