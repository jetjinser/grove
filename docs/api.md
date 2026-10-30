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
| Write a Forester page | `@use-modules[(forester commands)]` for Forester metadata and tree commands; core Scribble syntax comes from the language | a page needs project-local `.plant` modules; load them with `(grove input scribble)` |
| Reuse a named page command vocabulary | `@use-commands{manifest}`; the manifest exports `command-modules`; see [command sets](authoring.md#command-sets) | use `@use-modules` for raw imports or one-off modules |
| Write bilingual Forester content | `(forester i18n)` → `lang`, `bilingual`, `term`; import it in the page's command manifest or with `@use-modules`; see the [localization guide](i18n.md#bilingual-content) | compose `(forester style i18n)` → `default-bilingual-style` into site-wide HTML styles; use Content IR styles for element-level changes |
| Configure the normal Forester site | `(forester project)` → `make-forester-project` | the reader, analysis, or complete renderer set needs replacing |
| Share a site origin and default locale across outputs | `(forester render site)` → `make-site-settings`; see [site outputs](site-outputs.md) | renderer-specific options override shared values when one output needs a different setting |
| Apply a reusable style to content | `(grove style)` → `make-content-style`, `merge-content-style`; pass it through `#:style`; use `(grove style html element)` for HTML attributes and tag markers | the style value is format-neutral; HTML interprets it as classes and element properties, unlike renderer-wide `(grove style html)` stylesheets; see [tree composition](forester-composition.md#element-styles) |
| Change Forester's site-wide stylesheets | `(forester style html)` → `default-html-style`; compose with `(grove style html)` → `compose-html-styles` and pass through HTML presentation `#:style`; see [tree composition](forester-composition.md#styles) | styles apply to the whole HTML document, while Content IR `#:style` applies to individual elements |
| Adapt Scribble documents to Forester tree pages | `(forester input scribble)` → `make-page-reader`; use `document->tree-page` when the document is already read | the project needs a different page model or metadata policy |
| Parse Scribble syntax in a custom reader | `(language scribble parser)` → `scribble-parse`, `scribble-read` | to evaluate a document into Content IR in a supplied Guile module, use `(language scribble spec)` → `scribble-evaluate-document`; standard pages should use the Grove or Forester reader |
| Build a site from page documents | `(grove project site)` → `make-site-project` | the input is not a page or the model is project-specific |
| Assemble a page model directly | `(grove site)` → `make-site-page`, `make-site` | the project should collect and read source pages itself |
| Declare all sources of a site-derived artifact | `(grove site)` → `site-source-paths` | attach the result to an artifact's `#:sources`; declare extra files to the project separately when Grove must track them |
| Build an arbitrary file-based model | `(grove project)` → `make-project` | custom collection, reading, analysis, artifacts, or execution options are needed |
| Validate a project without publishing | `(grove project)` → `check-project` | use `build-project` to publish outputs or `build-project/report` to inspect source changes and cache status |
| Load a project file from a Scheme host | `(grove project load)` → `call-with-loaded-project` | the host already owns the project value; construct it with `(grove project)` |
| Apply renderers directly to an existing model | `(grove render)` → `render-model` | input collection, caching, validation, or output writing is needed; use a project |
| Create a source for a project | `(grove project)` → `create-project-source`, or the `grove new` command | the project must supply its own `#:create-source` policy |
| Collect files recursively | `(grove collect)` → `collect-directory` | Forester can accept an ordered `#:collect-sources` callback while retaining its defaults; custom site projects can use the same option |
| Handle project failures in a Scheme host | `(grove project diagnostic)` for structured errors and Grove's formatter | custom work needs to add a stage or path; use `call-with-project-stage` |
| Add a terminal browser to a Forester project | `(forester browse)` → `make-forest-browser` | the project needs its own `#:browse` callback |
| Customize Gemtext output paths | `(forester render gemtext)` → `make-gemtext-routes` | use `(forester render text routes)` only when intentionally sharing a common text route value |
| Change how content renders | the format's renderer options and `(grove render rules)` | the project needs a new output format or a different build stage |
| Define a reusable custom HTML element command | `(grove render html element)` → `make-element` | the element needs semantic data or a non-HTML interpretation; use `(grove ir)` and add render rules as needed |
| Construct a titled disclosure block | `(grove ir)` → `make-content-disclosure` | Forester trees use `(forester commands)` → `subtree`; see [tree composition](forester-composition.md) |
| Rewrite content before analysis | `(grove content transform)` → `compose-content-transforms`; project constructors accept `#:transforms` | the behavior depends on the analyzed site model; use a renderer or render rule |
| Inspect a project's selected source paths | `(grove project)` → `collect-project-sources` | the source list depends on a directory filter or custom collector; use a build report for sources after the full pipeline |
| Localize interface text and formatting | `(grove i18n)` plus renderer `#:locale` and `#:messages`; see the [localization guide](i18n.md) | project-specific content should be translated; keep that data in the content model |
| Parse, resolve, inspect, or convert URIs and IRIs | SRFI 275 modules `(srfi :275 iri)`, `(srfi :275 uri)`, `(srfi :275 normalize)`, `(srfi :275 path)`, and `(srfi :275 utils)` | keep resource identity comparisons and scheme-specific retrieval rules at their own boundaries; see [URI and IRI operations](customizing.md#uri-and-iri-operations) |
| Pass explicit state through a render pass | `(grove render context)` → `make-render-context`, `render-context-with`; Forester renderers can use `(forester render context)` → `make-site-render-context` | renderer options configure a renderer; context carries values to rules and custom rendering code |
| Add an output derived from the analyzed model | a renderer in `#:additional-renderers` or `#:renderers` | the output depends only on declared project files, independently of the model; use `#:pipelines` |
| Generate artifacts from declared files and tools | `(grove pipeline)` → `make-asset-pipeline`, then pass it through project `#:pipelines`; see [tool integration](customizing.md#integrate-an-external-tool) | output depends on the analyzed model; use a renderer instead |
| Construct a renderer's output artifacts | `(grove output)` → `make-artifact`, `make-byte-artifact` | output comes from declared files independently of the model; use an asset pipeline |
| Publish artifacts outside a project build | `(grove output)` → `write-artifacts`; use `validate-artifacts` to check a complete set before writing | a normal Grove project already validates and publishes its artifacts |
| Copy a static asset directory into the build | `(grove render static)` → `make-static-assets-renderer` | files are generated from declared inputs or a tool; use an asset pipeline |
| Render alternate image encodings as HTML `<picture>` | `(grove render html image)` → `make-image-set`, `image-set-rule` | the encoded files still come from a project-owned asset pipeline |
| Configure code highlighting | `(grove highlight)` → `make-highlighter`, `default-highlighters`; HTML `#:highlighters` | use `(grove style highlight)` → `default-highlight-style` to compose its stylesheet; see [syntax highlighting](project.md#highlighting) |
| Run a tool while reading a page | Scheme in `.plant`; use `depend-on!` from `(grove input)` for files discovered by one evaluation, or `(grove input scribble)` → `make-scribble-document-reader #:cache-dependencies` for fixed reader-wide files; use `disable-cache!` from `(grove input)` for effects that must repeat | use project `#:extra-sources` when the analyzer also needs those paths in `project-context-extra-sources`; use an asset pipeline for standalone artifacts |
| Reuse derived bytes inside an extension | `(grove cache)` → `make-content-cache` | cache-aware renderer artifacts and pipeline outputs are cached by the project |
| Write or extend math | `(grove math formula)` for text-based writing and calculations; `(grove math semantic)` for typed values; `(grove math notation)` for display layout; see the [Math reference](math.md) | inspect results with `(grove math report)` and `(grove math diagnostic)`; verify or audit derivations with `(grove math verification)` and `(grove math audit)`; compose passes with `(grove math pass)` or query rule relations |
| Independently check exact polynomial claims | `(grove math semantic-check)` → `math-polynomial-check-rule-spec`, `math-polynomial-check-derivation`, `math-polynomial-check-derivative` | use `(grove math verification)` to replay registered rewrite steps; the polynomial checker covers a separate, smaller semantic domain |
| Present math reports as Grove content | `(grove math presentation)` → `math-finished-report->content`, `math-verification->content`, `math-derivation->content` | Formula's `derive->content` is the short path when text input and its default presentation are sufficient |
| Inspect mathematical source positions and derivation origins | `(grove math provenance)` → `math-origin-location`, `math-origin-locations`, `math-location-file/line/column` | diagnostics and derivation records retain provenance; this module interprets it without requiring the calculation API |
| Scope mathematical assumptions in Scheme | `(grove math context)` → `with-math-assumptions`, `math-assumptions` | article commands use `@with-assumptions`; individual calculations accept `#:assumptions` |
| Inspect or transform symbolic math terms | `(grove math term)` → `math-term?`, `math-term->source`, `math-term-size`, and operand-path operations; see the [Math reference](math.md) | ordinary writing and calculations should stay with Formula or Semantic values |
| Construct and check proof evidence | `(grove math derivation)` for candidate chains; `(grove math verification)` to replay them; `(grove math proof)` for explicit proof trees | combine replay, contract checks, and proof elaboration with `(grove math audit)` |
| Compose mathematical type constraints | `(grove math types)` → `math-typeo`, `math-infer-type` | ordinary calculations infer types automatically; use the relation when composing custom math goals |
| Convert a math value to renderable content | `(grove math layout)` → `math-value->layout`; use `math-notation->datum` to inspect the resulting notation tree | use `(grove math notation)` to author display-only layout |
| Query finite relations | `(grove relation)` for relation values and miniKanren goals; `(grove relation dsl)` for named schemas and rules; see the [Relation reference](relations.md) | a domain API should return ordered, domain-specific results |
| Query Forester pages, content, references, and terms | `(forester relations)` for finite relation goals and row queries | use `(forester site)` when results should be page/content records in site order; see the [Relation reference](relations.md) |
| Represent and traverse tree composition facts | `(forester composition)` → `make-subtree-definition`, `make-transclusion`, `make-composition`, `composition-target-ids`; see [tree composition](forester-composition.md#composition-api) | query a built Forest in page order with `(forester site)` |
| Count user-perceived text units | `grapheme-count` from `(grove unicode)` | the task needs codepoint or UTF-8 byte counts instead |
| Match text independent of Unicode case and canonical spelling | `canonical-caseless-key` from `(grove unicode)` | sorting or locale-aware collation is also needed; use `(grove i18n)` → `sort-strings` |
| Run a live preview from Scheme | `(grove serve)` → `serve-project` | ordinary local previews can use the `grove serve` command |
| Serve an interactive browser over SSH from Scheme | `(grove ssh)` → `serve-ssh`, `read-authorized-keys`, `generate-host-key`; available when `guile-ssh` is installed; `serve-ssh` defaults to `127.0.0.1:2222` | the CLI's `grove browse --ssh` path is covered in [Preview and progress](project.md#preview-and-progress) |

`make-forester-project` supplies the normal reader, forest analysis, default
HTML renderer, and source-creation behavior. Configure the default HTML with
`#:html-options`, transform pages before analysis with `#:transforms`, and add
peer outputs with `#:additional-renderers`. These changes do not require
reassembling the standard Forester reader and analyzer.
`make-site-project` starts from the page and Content IR model, then exposes the
complete renderer set and other lifecycle stages for composition. Its
`#:analyze` callback receives the assembled site and may return another model;
it has no default output renderer, so pass the complete `#:renderers` list.
Forester's project constructor supplies the default HTML renderer and takes
extra outputs as peers. `make-project` is the general lifecycle and does not
require Grove's Content IR. See the
[project reference](project.md) for callback contracts and examples.

For renderer extensions, a plain procedure from model to artifact list is the
default and runs on each build. Wrap it with `make-cached-renderer` to reuse
artifacts; provide fixed unique `#:dependencies` paths or a collector
returning unique paths, and `#:configuration` for state Grove cannot observe
through the model and tracked project files. Configuration must be acyclic
Scheme data composed of booleans, characters, numbers, symbols, keywords,
strings, bytevectors, pairs, and vectors; a model-to-data procedure may compute
it dynamically.
Opaque objects and cyclic structures cannot provide stable cache identity.
`make-render-rules` is narrower: it changes the interpretation of content kinds
inside an existing renderer. See the [customization guide](customizing.md) for
how these layers compose. Forester renderers can use `(forester site)`'s
`tree-occurrences`, `tree-occurrence-target`, `page-source-paths` (by page ID),
`source-paths`, and `used-codepoints` projections instead of rebuilding tree
structure, dependency closure, or character inventories; see
[tree composition](forester-composition.md#renderer-queries).

## Site model

`(grove site)` provides `make-site-page` and `make-site` for code assembling a
page-based model directly. A site page holds an ID, title, Content IR document,
an optional `#:source` path, and optional `#:metadata`. The site holds the
ordered pages, descriptive fields, and an optional home page ID. Its `#:sources`
list declares site-wide dependencies that renderers may attach to aggregate
outputs; it does not configure project input collection. `site-sources` returns
only those site-wide dependencies. `site-source-paths` combines them with each
page's primary source and source paths recorded in page content, providing a
convenient dependency closure for aggregate artifacts. It does not add
otherwise unknown paths to a project's tracked input set; use `#:extra-sources`,
renderer `#:dependencies`, or the input tracker for that. Page IDs must be
unique, and a supplied home ID must name one of the pages. `make-site-project`
builds these values from collected inputs; use the constructors directly when
synthesizing pages or composing a custom analysis stage.

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
are the typed extension surface. `(grove math calculation)` is the procedural
calculation facade: it also re-exports the report, diagnostic, derivation,
verification, and audit accessors used to inspect its results. Import
`(grove math provenance)` for source locations and derivation origins; import
`(grove math report)`, `(grove math diagnostic)`, or another focused module
when code needs only that layer. The rule, proof, and audit
modules expose lower-level calculation machinery; ordinary article authors
should not need them.

## Content values

Content text, layout, element, document, and style records are values. Their
constructors copy mutable strings, bytevectors, vectors, and pair structure,
and their public accessors return independent copies of mutable data. Changing
a string or list returned by an accessor does not alter the node. Use
`content-with-style` and `content-with-property` to derive an element with an
intentional change;
`content-element-with-children` replaces its children while preserving its
kind, source, properties, style, and flow behavior.
`content-element-has-property?` distinguishes an absent property from a
property explicitly set to `#f`, matching render-context key semantics.
`make-content-disclosure` constructs a titled block disclosure with an
optional initial `open?` state and accepts `#:source` like other content
elements. It has no tree or route semantics; Forester's authoring layer owns
those concepts. Forester recognizes the disclosure inside a math derivation
as a subtree so the full steps can be queried or transcluded.
Opaque extension values embedded in properties remain shared and follow their
own type's contract.

Content source locations are association lists with unique symbol keys. The
`file` value, when present, is a non-empty filesystem path string without NUL;
`line` and `column` are positive one-based integers. Text constructors
validate and snapshot a supplied `#:location`;
`content-with-source-location` applies the same contract when attaching a
location to text or an element. Given a document or nested content sequence,
it recursively annotates each contained content value while preserving the
sequence shape. When text has no explicit `source`, annotation also uses the
location's `file` as its text source; an existing source is preserved. Elements
keep their explicit source separate from their location. Documents have no
location field; elements accept `#:location` in `make-content-element`.
`source-location` is reserved
and cannot be stored as an ordinary element property. Use
`content-source-location` to read either kind uniformly. It returns `#f` for
strings, layout nodes, and documents. Non-content Scheme values pass through
`content-with-source-location` unchanged, which lets Scribble annotate
arbitrary expressions safely. Constructors for content text, layout, elements,
and documents accept an optional `#:source` path for provenance and dependency
tracking. It must be `#f` or a non-empty filesystem path without NUL. This is
separate from `#:location`, which records a diagnostic position and may name a
different file.
`content-source-paths` walks an IR value or nested sequence of content values,
including documents, and returns unique paths from both provenance and
location file fields in traversal order. Use it when content-derived artifacts
need to track their origins.

`(grove ir)` defines these content values and their structural operations.
Use `make-content-text` for prose. `make-content-layout` represents formatting
that a reader needs to preserve separately from prose; Scribble's flow decoder
recognizes layout kind `'newline` as a line or paragraph boundary, while other
layout kinds have no built-in kind-specific behavior and carry textual content.
Most content extensions do not need to construct layout nodes directly.
Text has two intentionally different projections. Use
`(grove ir plain-text)` for a readable projection of a content value
without document-level formatting, including human-oriented notation for
structured math. It does not preserve all content semantics and is not a
serialization format.
Use `(grove render text)` to interpret content as rendered text: it adds
document formatting, accepts render rules and context, and is the basis for
Forester's text and Gemtext outputs. Choose it when output behavior or custom
content kinds matter.

Scribble forms may return content values or recursively nested lists of them;
the reader flattens those sequences and decodes prose into paragraphs. The
`content-value?` and `content-sequence?` predicates in `(grove ir)` define
those shared IR contracts; `(language scribble flow)` re-exports them and adds
`flatten-content-sequence`. Authors building custom block commands can use
`decode-flow` there to turn command bodies into paragraphs around blank lines
while keeping block elements outside paragraphs. Variadic command procedures
can use `command-body` from `(language scribble command)` to remove
keyword/value options while preserving body order. `make-content-element`
defaults its independent `#:flow-kind` to `inline`; set it to `block` when a
custom element should flush the current paragraph during `decode-flow`.

`content->sxml` applies a content element's HTML style automatically when a
render rule returns one root element. A custom HTML renderer assembling its
own root can use `content-style-html-attributes` from
`(grove render html style)` to apply the same class, attributes, and
scoped IDs. The same module exposes `content-style-html-tag-name` and
`content-style-html-void?` so an alternate HTML renderer can reuse Grove's
tag precedence and custom-element void semantics. `merge-html-attributes`
combines style attributes with renderer-owned attributes while protecting
renderer-owned IDs. These helpers are for custom SXML assembly; ordinary
render rules do not need to call them. Use `sxml->html` from
`(grove render html)` to serialize the result: it applies HTML void-element
and raw-text rules that a generic XML serializer does not.

Artifacts use the same ownership rule: `make-artifact` and
`make-byte-artifact` snapshot their path, content, and optional `#:sources`
paths. The `artifact-content` accessor returns a fresh string or bytevector.
`prefix-artifacts` places a validated artifact set under a relative output
directory while preserving each artifact's content and source dependencies.
Artifact paths reject absolute paths, empty or traversal components, and NUL.
For a Scheme host that owns publication, `write-artifacts` validates and writes
an artifact list to an output directory, removing only stale outputs recorded
in Grove's manifest. Pair it with `render-model` for a direct render-and-write
flow. Project builds perform validation and publication automatically and
remain the recommended entry point when Grove should also collect sources,
analyze content, and coordinate renderer and pipeline caches.

The artifact `#:sources` option maps an artifact to the unique tracked input
paths that affect it; it does not register those paths as project inputs.
Register files through the reader input tracker, project `#:extra-sources`, or
renderer `#:dependencies` as appropriate for the stage that reads them.
Duplicate dependency paths are rejected. Omitting `#:sources` means its
dependencies are unknown (`#f`), so Grove handles the artifact conservatively
during invalidation and incremental rendering. Pass an explicit empty list
when the artifact is known to depend on no input files; pass the relevant paths
when it depends on only part of the project. This mapping lets incremental
renderers reuse unaffected artifacts safely.

HTML presentation, home-link, and navigation-link values apply the same rule
to their SXML, labels, IDs, and attribute lists. Their accessors return copies
of mutable data, so editing a returned fragment does not change the configured
presentation.
