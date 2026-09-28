<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: CC0-1.0
-->

# A small category theory forest

This example adapts the `cat-0001.tree` and `cat-0002.tree` notes from
[`~/vie/writing/forest`](~/vie/writing/forest). That repository's `REUSE.md`
places everything under `trees/` under CC0-1.0. The text here is abridged and
adapted into Grove's `.grv` syntax; spelling and phrasing have been cleaned up.

The category page defines the composition axioms as an addressable subtree.
The remarks page links back to the definition and transcludes those axioms.
This adds a composition example to the source material's original
cross-reference pattern.

Build it from the Grove repository root:

```sh
guile -L modules examples/forest-category/build.scm
```

Outputs go to `/tmp/grove-forest-category`. Pass a different directory as the
first argument to the build script to choose another output location.
