<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: CC0-1.0
-->

# A small category theory forest

This example adapts the `cat-0001.tree` and `cat-0002.tree` notes from
[`~/vie/writing/forest`](~/vie/writing/forest). That repository's `REUSE.md`
places everything under `trees/` under CC0-1.0. The text here is abridged and
adapted into Grove's `.plant` syntax; spelling and phrasing have been cleaned up.

The category page defines the composition axioms as an addressable subtree.
The remarks page links back to the definition and transcludes those axioms.
This adds a composition example to the source material's original
cross-reference pattern.

Build it from the Grove repository root:

```sh
grove build --project examples/showcases/forest-category/project.scm
```

Outputs go to this example’s `build/` directory. Use `--output PATH` to
choose another location, or `grove serve --project examples/showcases/forest-category/project.scm`
to preview it locally.
