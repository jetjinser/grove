<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Authoring `.plant` pages

A page is a Scribble document.  See [Design boundaries](design.md) for what
Grove owns versus what a project supplies. Commands use `@name{body}` or
`@name[arguments]{body}`; metadata is written with the same command shape.

## Minimal page

```scribble
@use-modules[(forester commands)]

@title{A short note}
@summary{One sentence for descriptions and feeds.}
@published{2026-10-21}
@tag["example"]

@paragraph{The body starts here.}
```

## Metadata

| Command | Meaning |
| --- | --- |
| `@title{...}` | Page, section, or subtree title; each requires exactly one direct title. |
| `@summary{...}` | Description used by `<meta>`, Open Graph, and feeds. |
| `@og-image{assets/cover.png}` | Open Graph image; external HTTP(S) URL/IRI values pass through, relative paths use the site origin and HTML routes. |
| `@og-image-alt{...}` | Alternative text for `@og-image`; defaults to the page title. |
| `@published{YYYY-MM-DD}` | Valid Gregorian publication date. |
| `@updated{YYYY-MM-DD}` | Valid Gregorian update date; must not precede `@published`. |
| `@describes{absolute-iri}` | External entity this tree describes; repeat for multiple entities. |
| `@taxon["chapter"]` | One classification per page, rendered above the title. |
| `@tag["i18n"]` | Additional labels; repeat as needed. |
| `@locale["zh-Hans"]` | Page language and direction. |
| `@translation["intro"]` | Translation group key shared by parallel pages. |

Page metadata commands may appear anywhere among that tree's top-level forms;
nested subtrees have their own metadata scope. Keeping page-level metadata at
the top makes the source readable and lets tools find it quickly. A second
`@taxon` is an error; use `@tag` for additional labels. Topic values for both
commands must start and end with a letter or digit and contain only letters,
digits, and hyphens; Grove stores them in lowercase.

`@describes` records an external entity IRI, separate from the page's
own URL and from ordinary links. For example, a literature page can declare
`@describes{doi:10.1146/annurev.fluid.29.1.123}` or a Unicode IRI such as
`@describes{https://例え.テスト/资料}`. Grove retains the authored IRI spelling
for queries and compares declarations exactly, so equivalent spellings such
as Unicode and percent-encoded forms are distinct; choose one spelling
consistently. Grove links HTTP(S) IRIs directly and presents DOI IRIs through
`https://doi.org/`. Other absolute IRIs remain visible as identifiers without
an assumed resolver. Values must be valid absolute IRIs, cannot contain
userinfo credentials, and HTTP(S) values require a host. HTML keeps HTTP(S)
IRIs for the browser to process; Gemtext converts link targets to ASCII URI
form. Grove does not fetch metadata from the identifier. The same validation
applies when constructing a Forest directly from Grove site values; the
generic `(grove site)` page constructor leaves metadata application-defined.

## Content

```scribble
@paragraph{Prose with a @link["other-page"]{link} and @emph{emphasis}.}

@image["forest.jpg"]{A grove at dawn}

@code{(+ 1 2)}
@code-block[#:language "scheme"]{(define (square x) (* x x))}
@verbatim["literal text, no code semantics"]

@itemize{
  @item{First}
  @item{Second}
}
```

- `@image["src"]{alt}` takes its alternative text from the children. Leave the
  children empty for a decorative image.
- `@code` is inline; `@code-block` is a highlighted block and accepts the
  language as a string or symbol; `@verbatim` is literal text, not code.
- `@section{@title{...} ...}` groups content structurally. It renders a
  heading and body but gets no route and no anchor. Use `@subtree` when the
  group must be linked.

## Subtrees

```scribble
@subtree[#:id "stable-id"]{
  @title{A section with an address}
  @paragraph{This section gets its own page and anchor.}
}
```

- `@subtree[#:id "..."]` gives the subtree an explicit, unique ID for links
  and transclusion.
- `@subtree{...}` without `#:id` receives a position-dependent `unstable-N`
  id. That is fine for pure structure, but never link to it; use `@section`
  for grouping that needs no address.
- Every subtree needs exactly one direct `@title`.

## Language

```scribble
@bilingual{
  @lang["en"]{The grove is a small forest.}
  @lang["zh-Hans"]{树林是一片小小的森林。}
}

@paragraph{Inline foreign text: @term["fr"]{une forêt}.}
```

- `@bilingual` folds the non-reader lanes by default; `@bi` is its built-in
  short form. Both use the project's `#:reader-locale` as the default reader.
- Pass `#:reader "en"` to override the reader for one block, or
  `#:mode 'parallel` for side-by-side lanes.
- Tags are canonicalized: prefer `zh-Hans`/`zh-Hant`; `zh-CN` and `zh-TW` are
  accepted aliases. See [i18n](i18n.md).

## Comments and escaping

- `@;` starts a line comment.
- Write a literal `@` as `@"@"`.
- Braces must balance; a missing brace is reported with its source location.

## Accessibility

- Give informative images alt text in the children; leave decorative images
  empty.
- Mark the language of foreign prose with `@lang` or `@term`.
- Keep one `@title` per page and use subtrees for nested headings.
- See [Accessibility](accessibility.md) for the renderer guarantees and the
  project checklist.

## Command sets

A page names the command vocabulary it uses:

```scribble
@use-commands{bilingual}
```

The name resolves to a module that exports a `command-modules` list:

```scribble
@(define-module (bilingual)
  #:export (command-modules))

@(define-public command-modules
   '((forester commands) (bilingual commands)))
```

`@use-commands` reads the list and imports each module into the page. The
manifest may live in a `.scm` module or, in a project that loads `.plant`
modules, in the project's module directory. A missing module, a missing
`command-modules` export, or a non-name argument is an error.

- `@use-commands{NAME}` is the semantic form: the page says which command set
  it speaks, and the manifest says which modules implement it.
- `@use-modules[...]` remains the raw form for arbitrary imports, prefixed
  modules, and one-off macros.
- There are no project-wide default modules. The page names the set, so a
  `.plant` read outside the project still shows its dependency; the manifest
  keeps the module list in one place.
- Include `(forester commands)` explicitly in the manifest. The base commands
  are part of the language, but the manifest is the readable list of
  everything the set provides.
- The manifest itself is imported into the page environment, so the build
  records its source as an input; a `.scm` manifest needs no separate project
  input declaration.
- The name may be a single name (`{bilingual}` or `[bilingual]`), or a
  multi-segment module name (`[(my project)]`).

This is not a Guile `define-language`. A Guile language selects the reader,
compiler, evaluator, and default environment, and is chosen by the CLI or the
project. A command set only selects document vocabulary, and is chosen by the
file. The Scribble language remains the only Guile language involved.

