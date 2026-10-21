<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Authoring `.plant` pages

A page is a Scribble document. Commands use `@name{body}` or
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
| `@title{...}` | Page or section title; required for a page and for every subtree. |
| `@summary{...}` | Description used by `<meta>`, Open Graph, and feeds. |
| `@published{YYYY-MM-DD}` | Publication date. |
| `@updated{YYYY-MM-DD}` | Last update; must not precede `@published`. |
| `@taxon["chapter"]` | One classification per page, rendered above the title. |
| `@tag["i18n"]` | Free labels; repeat as needed. |
| `@locale["zh-Hans"]` | Page language and direction. |
| `@translation["intro"]` | Translation group key shared by parallel pages. |

Metadata may appear anywhere, but keeping it at the top makes the page
readable and lets tools find it quickly. A second `@taxon` is an error; use
`@tag` for additional labels.

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

- `@subtree[#:id "..."]` is addressable and should carry a stable, unique id.
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

- `@bilingual` folds the non-reader lanes by default; `@bi` is the project
  alias, and both use the project's `#:locale` as the default reader.
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

## Modules

Each page declares the commands it uses:

```scribble
@use-modules[(forester commands) (my project commands)]
```

A project can define short aliases in a shared module (for example `@bi`) and
re-export them; the module is loaded once per page. Project-level default
modules are not built in yet, so the `@use-modules` line remains per page.
