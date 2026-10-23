<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Customizing a site

This is the shortest path from “I want to change X” to the right Grove API.

## Decision table

| I want to change… | Use | Where |
| --- | --- | --- |
| one element's HTML | render rules | `make-html-options #:rules (make-render-rules ...)` |
| one element's text/gemtext output | the same rule value | `make-text-options` / `make-gemtext-options #:rules` |
| the metadata line under a title | `metadata-items` hook | `make-html-hooks #:metadata-items` |
| the site navigation | `site-nav` hook | `make-html-hooks #:site-nav` |
| the home link | `home-link` hook | `make-html-hooks #:home-link` |
| tree headings or the table of contents | `title-decoration` hook | `make-html-hooks #:title-decoration` |
| locale, messages, or date formatting | renderer options | `make-html-options #:locale/#:messages` |
| output paths | routes | `make-html-routes`, `make-text-routes`, `#:prefix` |
| sitemap, feed, or graph | derived artifacts | `make-site-options #:derived` |
| content before rendering | content transforms | `make-site-options #:transforms` |
| static files or generated assets | a renderer or pipeline | `make-static-assets-renderer`, `#:pipelines` |
| a new output format | a renderer | `(lambda (forest) ...)` + `make-cached-renderer` |
| build/cache behavior | project options | `make-site-options #:cache?`, `#:output-directory` |

If the change is about *how one node is interpreted*, use rules.  If it is
about *the environment of a render pass*, use a context.  If it is about
*reusable output policy*, use options or presentation.  If it is about
*rewriting content*, use a transform.

## Three renderer entry points

| Function | Returns | Use when |
| --- | --- | --- |
| `site->html` | alist of page id → HTML string | tests, inspection, small tools |
| `render-html` | list of artifacts | custom build pipelines without project caching |
| `make-html-renderer` | cached renderer | `make-site-project #:renderers` |

Text and gemtext follow the same shape: `site->text`, `render-text`,
`make-text-renderer`; `site->gemtext`, `render-gemtext`,
`make-gemtext-renderer`.

All of them take one options value:

```scheme
(site->html forest #:options (make-html-options ...))
(render-html forest #:options (make-html-options ...))
(make-html-renderer #:options (make-html-options ...))
```

## Options vs context

| | options | context |
| --- | --- | --- |
| lifetime | immutable configuration | one render pass |
| shape | typed record | symbol alist with typed accessors |
| read by | renderer entry points | rules and derived artifacts |
| examples | style, rules, hooks, routes | forest, page, locale, messages |

Use `make-html-options` / `make-text-options` / `make-gemtext-options` for
configuration.  Use `render-context-page`, `render-context-locale`, and the
other typed accessors inside a rule.  `render-context-ref` remains available
for project-defined keys.

## Minimal examples

### Change one image rule

```scheme
(define rules
  (make-render-rules
    (list (cons 'image
                (lambda (context element children next)
                  (if (string=? (content-element-property element 'source)
                                "forest.jpg")
                      (picture-from-variants ...)
                      (next)))))))

(make-html-options #:rules rules)
```

### Add a metadata item

```scheme
(define hooks
  (make-html-hooks
    #:metadata-items
    (lambda (page)
      (append (default-metadata-items page)
              (list `(li (@ (class "meta-item")) "author"))))))

(make-html-options #:hooks hooks)
```

### Add sitemap and feed

```scheme
(make-site-project
  #:options
  (make-site-options
    #:derived
    (list (sitemap-derived #:routes routes)
          (rss-derived #:routes routes))
    #:output-directory "build"))
```

### Rewrite content before rendering

```scheme
(make-site-project
  #:options
  (make-site-options
    #:transforms (list (compose-content-transforms
                         merge-adjacent-paragraphs
                         enrich-image-variants))
    ...))
```

## Where to read next

- [renderer-rules.md](renderer-rules.md) — rules, composition, `next`.
- [render-context.md](render-context.md) — contexts and the four context kinds.
- [derived-artifacts.md](derived-artifacts.md) — sitemap, RSS, graph.
- [project.md](project.md) — project options, cache, CLI.
- [composition.md](composition.md) — the algebra behind the layers.
