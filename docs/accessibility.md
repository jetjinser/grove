# Accessibility

Grove's renderers ship a small, always-on accessibility shell and leave
content semantics to the project.  This page states what the HTML renderer
guarantees, what a project must supply, and how the i18n surface feeds into
it.  Text and Gemtext output are plain text; the HTML shell does not apply
to them.

## What the HTML renderer guarantees

- `<main id="main" tabindex="-1">` wraps the page body, and every document
  starts with a skip link to `#main`.
- The site header is a `<nav>` with an accessible name; the table of
  contents is a named `<nav>`; backmatter is an `<aside>`.
- A language switcher, when enabled, is a named navigation whose current
  language carries `aria-current="true"`.
- The site navigation link to the current page carries
  `aria-current="page"`.  Use `site-nav-link` for project navigation, and
  pass `#:site-nav` a procedure of the current page id so it can mark the
  active entry.
- `@image` emits `<img>` with `alt` taken from the element's children; an
  image without children is decorative (`alt=""`).
- Collapsible trees and backmatter use native `<details>/<summary>`, so
  keyboard operation and expanded state come from the browser.
- Dates render as `<time datetime="…">` with a human-readable label.
- The default stylesheet hides the skip link until it is focused and keeps
  `:focus-visible` outlines visible.

## Chrome labels

Every built-in label (site navigation, contents, skip link, tree link,
updated date, backmatter headings) comes from a message catalog; the text and
Gemtext renderers use the same catalog for their Home and Contents labels.  The
built-in catalog covers `en` and `zh-Hans`; select it with `#:locale` and
extend it with `#:messages`:

```scheme
(make-html-renderer
  #:locale "zh-Hans"
  #:messages (make-message-catalog
              '(("zh-Hans" . ((nav-site . "站内导航"))))
              #:default "zh-Hans"))
```

A partial catalog is layered over the built-in one, so keys it does not
name keep their default text.  The same catalog drives the backmatter
headings; `#:backmatter-labels` remains available as an explicit override.

## What a project supplies

- **Page language and direction**: pass `#:locale` and the renderer derives
  `lang`/`dir`; `#:html-attributes` entries override the derived values.
  Without `lang`, screen readers guess the pronunciation.
- **Navigation**: build entries with `site-nav-link`, or add
  `aria-current` yourself.  The renderer cannot infer which project link
  points at the current page.
- **Rich content**: tables, forms, and other interactive components are
  authored as raw HTML or project renderers; their roles, labels, and
  keyboard behaviour are the project's responsibility.
- **Contrast and motion**: the default stylesheet respects
  `prefers-reduced-motion`; custom styles must keep contrast and focus
  indicators.

## Checklist

Before publishing a site:

1. Tab through the page: the skip link appears first, focus is always
   visible, and every disclosure opens with Enter or Space.
2. Read a page with a screen reader: the document language is correct,
   the navigation and contents landmarks are announced, and the current
   page is marked.
3. Zoom to 200% and reflow to a narrow viewport: no content is clipped and
   no horizontal scrolling is required for prose.
4. Check images: informative images have text in their children,
   decorative ones are empty.
5. Run the a11y suite (`blue check tests/unit/forester/a11y.scm`) after
   changing the document shell.

## WCAG mapping

| Requirement | Where it is met |
| --- | --- |
| 1.3.1 Info and relationships | `main`, `nav`, `aside`, headings, `details` |
| 2.1.1 Keyboard | native controls, skip link |
| 2.4.1 Bypass blocks | skip link |
| 2.4.7 Focus visible | default `:focus-visible` rule |
| 2.4.8 Location | `aria-current="page"` on navigation |
| 3.1.1 Language of page | `#:html-attributes` / locale |
| 3.1.2 Language of parts | bilingual lanes carry `lang` |
| 4.1.2 Name, role, value | named landmarks, native disclosures |

Language metadata is the overlap between i18n and accessibility; see
[i18n](i18n.md) for locales, catalogs, and matching.
