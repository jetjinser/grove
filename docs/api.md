<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# API map

Grove is a Scheme platform, so the project file and `.plant` pages can use
ordinary Guile and SRFI code. Start at the highest layer that already matches
the work; drop lower only when you need to replace part of its behavior.

| Task | Start here | Reach lower when… |
| --- | --- | --- |
| Write a Forester page | `(forester commands)` and `(language scribble commands)` in `.plant` | the page needs project-local Scheme modules or a new command vocabulary |
| Configure the normal Forester site | `(forester project)` → `make-forester-project` | the reader, analysis, or complete renderer set needs replacing |
| Adapt Scribble documents to Forester tree pages | `(forester input scribble)` → `make-page-reader` | a custom page project needs Forester metadata extraction without the standard project lifecycle |
| Parse Scribble syntax in a custom reader | `(language scribble parser)` → `scribble-parse`, `scribble-read` | standard pages should use the Grove or Forester reader to convert forms to Content IR |
| Build a site from page documents | `(grove project site)` → `make-site-project` | the input is not a page or the model is project-specific |
| Assemble a page model directly | `(grove site)` → `make-site-page`, `make-site` | the project should collect and read source pages itself |
| Build an arbitrary file-based model | `(grove project)` → `make-project` | custom collection, reading, analysis, artifacts, or execution options are needed |
| Apply renderers directly to an existing model | `(grove build)` → `render-model` | input collection, caching, validation, or output writing is needed; use a project |
| Create a source for a project | `(grove project)` → `create-project-source`, or the `grove new` command | the project must supply its own `#:create-source` policy |
| Collect files recursively | `(grove collect)` → `collect-directory` | Forester can accept an ordered `#:collect-sources` callback while retaining its defaults; custom site projects can use the same option |
| Handle project failures in a Scheme host | `(grove project diagnostic)` for structured errors and Grove's formatter | custom work needs to add a stage or path; use `call-with-project-stage` |
| Add a terminal browser to a Forester project | `(forester browse)` → `make-forest-browser` | the project needs its own `#:browse` callback |
| Change how content renders | the format's renderer options and `(grove render rules)` | the project needs a new output format or a different build stage |
| Define a reusable custom content element | `(grove element)` → `make-element` | the element needs custom interpretation in a renderer; add a render rule |
| Rewrite content before analysis | `(grove content transform)` → `compose-content-transforms`; project constructors accept `#:transforms` | the behavior depends on the analyzed site model; use a renderer or render rule |
| Localize interface text and formatting | `(grove i18n)` plus renderer `#:locale` and `#:messages`; see the [localization guide](i18n.md) | project-specific content should be translated; keep that data in the content model |
| Pass explicit state through a render pass | `(grove render context)` → `make-render-context`, `render-context-with`; Forester renderers can use `(forester render context)` → `make-site-render-context` | renderer options configure a renderer; context carries values to rules and custom rendering code |
| Add a model-derived output | a renderer in `#:additional-renderers` or `#:renderers` | the output depends on declared files or an external tool; use `#:pipelines` |
| Construct a renderer's output artifacts | `(grove output)` → `make-artifact`, `make-byte-artifact` | output comes from declared files independently of the model; use an asset pipeline |
| Copy a static asset directory into the build | `(grove render static)` → `make-static-assets-renderer` | files are generated from declared inputs or a tool; use an asset pipeline |
| Render alternate image encodings as HTML `<picture>` | `(grove render html image)` → `define-image-sets`, `image-set-rule` | the encoded files still come from a project-owned asset pipeline |
| Run a tool while reading a page | Scheme in `.plant`; use `depend-on!` from `(grove input scribble)` for file dependencies; see the [customization guide](customizing.md) | the tool should produce standalone artifacts from project files; use an asset pipeline |
| Reuse derived bytes inside an extension | `(grove cache)` → `make-content-cache` | cache-aware renderer artifacts and pipeline outputs are cached by the project |
| Write or extend math | `(grove math formula)` for text-based writing and calculations; `(grove math semantic)` for typed values; `(grove math notation)` for display layout; see the [Math reference](math.md) | inspect results with `(grove math report)` and `(grove math diagnostic)`; verify or audit derivations with `(grove math verification)` and `(grove math audit)`; compose passes or query rule relations |
| Query finite relations | `(grove relation)` for relation values and miniKanren goals; `(grove relation dsl)` for named schemas and rules; see the [Relation reference](relations.md) | a domain API should return ordered, domain-specific results |
| Count user-perceived text units | `grapheme-count` from `(grove unicode)` | the task needs codepoint or UTF-8 byte counts instead |
| Run a live preview from Scheme | `(grove serve)` → `serve-project` | ordinary local previews can use the `grove serve` command |

`make-forester-project` supplies the normal reader, forest analysis, default
HTML renderer, and source-creation behavior. Configure the default HTML with
`#:html-options`, transform pages before analysis with `#:transforms`, and add
peer outputs with `#:additional-renderers`. These changes do not require
reassembling the standard Forester reader and analyzer.
`make-site-project` keeps the page and Content IR model while exposing the
complete renderer set and other lifecycle stages for composition. It has no
default output renderer: pass the complete `#:renderers` list. Forester's
project constructor supplies the default HTML renderer and takes extra
outputs as peers. `make-project` is the general lifecycle and does not require
Grove's Content IR. See the
[project reference](project.md) for callback contracts and examples.

For renderer extensions, a plain procedure from model to artifact list is the
default and runs on each build. Wrap it with `make-cached-renderer` to reuse
artifacts; provide `#:inputs` and `#:configuration` for dependencies Grove
cannot observe through the model and tracked project files. `make-render-rules`
is narrower: it changes the interpretation of content kinds inside an existing
renderer. See the [customization guide](customizing.md) for how these layers
compose.

## Site model

`(grove site)` provides `make-site-page` and `make-site` for code assembling a
page-based model directly. A site page holds an ID, title, Content IR document,
an optional primary source path, and metadata. The site holds the ordered pages,
descriptive fields, an optional home page ID, and a separate list of site-wide
source dependencies that renderers can attach to aggregate outputs. This list
does not configure project input collection. Page IDs must be unique, and a
supplied home ID must name one of the pages. `make-site-project` builds these
values from collected inputs; use the constructors directly when synthesizing
pages or composing a custom analysis stage.

## API layers

The modules form three user-facing layers rather than one umbrella module:

- **Project entry points** provide defaults: `(forester project)` for the
  standard Forester site, `(grove project site)` for custom page-based sites,
  and `(grove project)` for arbitrary models.
- **Composition points** are the intended extension seams: renderer
  procedures, render rules, content transforms, Scribble modules, relations,
  and asset pipelines. They use ordinary Scheme procedures and lists unless a
  typed value adds validation or meaning.
- **Domain modules** expose the useful pieces of a subsystem independently.
  Math Formula is its writing facade; rule, proof, audit, and pass modules are
  lower-level interfaces for projects building their own workflows.

Modules whose name includes `private` are internal. Public domain modules
remain available for direct composition, but their presence does not make every
low-level binding a recommended starting point. This API map is the supported
route from a user task to the narrowest useful layer.

Private bindings may change with their implementations. Public content
interpreters accept render rules and context; stock renderers use private
dispatch details for tree traversal and format-specific behavior.

For math, Formula is the writing-oriented facade and Semantic constructors
are the typed extension surface. `(grove math pipeline)` and the rule, proof,
and audit modules expose lower-level calculation machinery for code that needs
to inspect and compose those results; ordinary article authors should not need
them.

## Content values

Content text, layout, element, document, and style records are values. Their
constructors copy mutable strings, bytevectors, and pair structure, and their
public accessors return independent copies of mutable data. Changing a string
or list returned by an accessor does not alter the node. Use
`content-with-style` and `content-with-property` to derive an element with an
intentional change;
`content-element-with-children` replaces its children while preserving its
kind, source, properties, style, and flow behavior.
Opaque extension values embedded in properties remain shared and follow their
own type's contract.

Content source locations are association lists with unique symbol keys. The
`file` value, when present, is a string; `line` and `column`, when present,
are positive one-based integers. Text constructors validate and snapshot a
supplied `#:location`, and `content-with-source-location` applies the same
contract when attaching a location to content.

`(grove ir)` defines these content values and their structural operations.
Text has two intentionally different projections. Use
`(grove render plain-text)` for a readable projection of a content value
without document-level formatting, including human-oriented notation for
structured math. It does not preserve all content semantics and is not a
serialization format.
Use `(grove render text)` to interpret content as rendered text: it adds
document formatting, accepts render rules and context, and is the basis for
Forester's text and Gemtext outputs. Choose it when output behavior or custom
content kinds matter.

Scribble forms may return content values or recursively nested lists of them;
the reader flattens those sequences and decodes prose into paragraphs. Authors
building custom block commands can use `(language scribble flow)`:
`decode-flow` turns command bodies into paragraphs around blank lines and
keeps block elements outside paragraphs, while `content-value?`,
`content-sequence?`, and `flatten-content-sequence` expose the same value and
sequence rules used by the reader. Variadic command procedures can use
`command-body` from `(language scribble command)` to remove keyword/value
options while preserving body order.

`content->sxml` applies a content element's HTML style automatically when a
render rule returns one root element. A custom HTML renderer assembling its
own root can use `content-style-html-attributes` from
`(grove render html attributes)` to apply the same class, attributes, and
scoped IDs; `merge-html-attributes`
combines them with renderer-owned attributes while protecting renderer-owned
IDs. These helpers are for custom SXML assembly; ordinary render rules do not
need to call them. Use `sxml->html` from `(grove render html)` to serialize the
result: it applies HTML void-element and raw-text rules that a generic XML
serializer does not.

Artifacts use the same ownership rule: `make-artifact` and
`make-byte-artifact` snapshot their path, content, and source paths. The public
`artifact-content` accessor returns a fresh string or bytevector.
`prefix-artifacts` places a validated artifact set under a relative output
directory while preserving each artifact's content and source dependencies.
Artifact paths reject absolute paths, empty or traversal components, and NUL.

The optional third constructor argument declares the input paths that an
artifact depends on. Omitting it means dependencies are unknown (`#f`), so
Grove handles the artifact conservatively during invalidation and incremental
rendering. Pass an explicit empty list when the artifact is known to depend on
no input files; pass the relevant paths when it depends on only part of the
project. This declaration is what lets incremental renderers reuse unaffected
artifacts safely.

HTML presentation, home-link, and navigation-link values apply the same rule
to their SXML, labels, IDs, and attribute lists. Their accessors return copies
of mutable data, so editing a returned fragment does not change the configured
presentation.
