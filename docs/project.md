<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Grove projects

`(grove project)` runs the common project pipeline:

1. Collect optional module sources and pass them to the configured module loader.
2. Collect page sources and read each one into a Grove site.
3. Pass the site to an optional library analyzer.
4. Run each renderer over the analyzed value, combine the artifacts, and write
   them to the configured output directory.

The project API does not choose a document language or a site model. The
reader, analyzer, and renderers are procedures supplied by the project. For
example, a linked-notes project can use Forester as its analyzer and renderer
library:

```scheme
(use-modules (forester input scribble)
             (forester render html)
             (forester render text)
             (forester site)
             (grove input scribble)
             (grove output)
             (grove project)
             (grove render html routes)
             (grove style html))

(define read-document (make-scribble-document-reader))
(define source-directory "source")
(define style (make-html-style))
(define routes (make-html-routes))

(define project
  (make-project
    #:title "Linked Notes"
    #:source-directory source-directory
    #:module-directory "source/modules"
    #:load-modules load-scribble-modules!
    #:read-page (make-forester-page-reader read-document)
    #:analyze make-forest
    #:renderers
    (list (make-forester-html-renderer
            #:style style #:routes routes #:cache-key "linked-html-v1")
          (make-forester-text-renderer
            #:prefix "text" #:cache-key "linked-text-v1"))
    #:output-directory "result"
    #:cache-key "linked-project-v1"))

(build-project project)
```

## Component contracts

| Component | Grove contract | Responsibility |
| --- | --- | --- |
| Collector | configured directories and filters → ordered paths | Grove owns file discovery and source fingerprints. |
| Reader | source path → `site-page` | A reader chooses the input language and may evaluate Scheme. |
| Analyzer | `site` → any model value | A library such as Forester builds links or other cross-page indexes. |
| Renderer | model value → list of artifacts | Each renderer owns one output format and declares its artifact dependencies. |
| Writer | artifacts → output directory | Grove owns unchanged-file writes, stale-file removal, and build state. |

The analyzer result is opaque to Grove. Multiple renderers receive the same
value and may produce unrelated output formats; the project does not require
Forester or HTML. `grove` owns collection and build orchestration. Forester
owns its forest/link model and its HTML/text renderers. Forester currently
uses Grove's site, IR, and artifact types, so it is a library layered on Grove
interfaces rather than a separately packaged implementation. Keep that
dependency explicit; split it into a neutral shared interface only if a
second host actually needs to construct or consume those values.

The default collector selects `.grv` files directly inside each configured
directory. Supply `#:source-filter` or `#:module-filter` to change that
selection. Module sources are loaded before page sources; `.grv` module files
declare their module with Guile's ordinary `define-module`. Page reading,
analysis, and rendering remain independent phases. With no analyzer, renderers
receive the Grove site directly.

`write-artifacts` keeps a `.grove-artifacts` manifest in the output directory.
On the next build it removes files listed in the previous manifest that are no
longer produced, while leaving files it did not manage alone. The manifest path
and `.grove-build-state` path are reserved for Grove metadata.

Pages may carry an optional source path. A built site carries its page and
module input paths. Projects can additionally declare external files with
`#:extra-sources`; Grove fingerprints these paths and includes them in build
reports. External files do not automatically become dependencies of every
artifact: declare relevant paths on each artifact, such as a stylesheet
artifact's source file.
Forester indexes depend on all site inputs; each rendered page depends on its
own source, incoming-link sources that supply backlink titles, and shared
Scheme module sources. Custom renderers can pass a source-path list as the
third argument to `make-artifact`; omitting it means dependencies are unknown.
This provenance lets Grove explain which outputs may be affected by source
changes. Grove still reads and analyzes the complete site on each build.

`build-project/report` returns a build report with added, modified, and removed
input paths plus potentially affected artifact paths. `build-project` remains
available and continues to return the artifact list. Outputs from renderers
that leave their dependencies unknown are always reported as affected. Writing
an artifact whose contents are already current leaves its file untouched.

Renderer reuse is opt-in. Wrap a deterministic renderer with
`make-cached-renderer`, giving it a key that covers its configuration, and set
`#:cache-key` on the project for the reader, analyzer, title, and other
project-level inputs. These are caller-managed version keys, not hashes of
Scheme procedures. Bump them when reader, analyzer, or renderer code or
configuration changes. File inputs declared with `#:extra-sources` are
fingerprinted automatically; cacheable renderers must not rely on side effects.
Grove reuses outputs only when source fingerprints and both keys match and the
saved output files still match their recorded contents. It still evaluates all
page Scheme code and runs analysis on every build. By default, input changes
run all renderers. A cached renderer can opt into partial updates with
`#:render-changed`; its callback receives the current model, changed source
paths, dirty old output paths, and its old artifacts. Return the complete
current artifact set for that renderer, reusing old artifacts where useful;
return `#f` to request a full render. New inputs make all that renderer's old
outputs candidates for replacement because they may add cross-page effects.
Callbacks that perform partial updates take responsibility for keeping their
complete output set correct. Cacheable renderers must not rely on skipped side
effects, and partial callbacks run only on source changes.

`make-forester-html-renderer` accepts an optional `#:cache-key`. With a key,
its HTML artifacts depend on the page itself and pages that link to it, plus
shared Scheme module sources. The incremental callback uses those dependencies
to preserve unrelated page output while refreshing indexes and pages whose
backlinks changed. The key must cover the renderer's routes, style, and
MathJax configuration.
`make-forester-text-renderer` provides the same opt-in caching and incremental
behavior, with an optional output `#:prefix` such as `"text"`.

The complete runnable example, including custom routes and CSS, is in
`examples/linked-notes/build.scm`.

Forester's HTML renderer uses MathJax 4's combined TeX-to-CommonHTML component
on pages that contain math. It loads the component with `defer`; projects can
pass `#:mathjax-src` to `make-forester-html-renderer` to use a locally hosted
copy or another compatible MathJax build, and can pass `#f` to disable it.
