<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Renderer rules

A renderer is an interpretation of the content algebra, not a set of
imperative hooks.  Content has constructors such as `paragraph`, `image`,
`link`, and `subtree`; a rule set maps each constructor to a function that
returns HTML.

## The model

```scheme
(define picture-rules
  (make-html-rules
    (list (cons 'image picture-rule))))
```

- A rule is a value: `kind -> (element children -> sxml)`.
- `compose-html-rules` is a left-biased merge: the overlay wins for a kind it
  interprets, and the base remains the fallback for everything else.
- `html-rules->handlers` adapts the value to `content->sxml`.
- `define-html-rules` is a thin macro that expands to `make-html-rules`; it
  adds no new evaluation model.

The laws the experiment checks:

1. **Identity**: composing with the empty rule set changes nothing.
2. **Associativity**: `a + (b + c) = (a + b) + c` under the same precedence.
3. **Override**: a later rule wins for its kind.
4. **Totality by fallback**: kinds without an overlay still use the base.
5. **Expansion**: the DSL produces the same values as the explicit API.

## Image example

Image variants are data, not a format policy in the core:

```scheme
(define variants
  '(("forest.jpg" . (("avif" . "/images/forest.avif")
                     ("jxl"  . "/images/forest.jxl")))))

(define (picture-rule element children)
  (let ((source (content-element-property element 'source))
        (alt (content-verbatim-string children)))
    (if (assoc-ref variants source)
        (picture-from-variants source alt)
        `(img (@ (src ,source) (alt ,alt))))))
```

The asset pipeline produces the variant files; the rule only turns the image
element into `<picture>`.  Grove never learns which codec or encoder is used.

## Experiment status

The candidate API lives in `(grove render html rules)`, with tests in
`tests/unit/grove/html-rules.scm`.  It is not wired into the default site
renderer yet; the experiment is intentionally isolated so the value model can
be evaluated before production adoption.

The next step, if adopted, is to replace the internal handler alists in
`site->html`/`render-html` with rule values and pass a render context
(forest, routes, page) to rules that need it.
