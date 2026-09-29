<!--
SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
SPDX-License-Identifier: GPL-3.0-or-later
-->

# Grove IR

Input languages produce Grove content; renderers consume it.

`(grove ir)` provides four content records:

| Record | Meaning |
| --- | --- |
| `content-text` | Text. |
| `content-layout` | Original whitespace. |
| `content-element` | Named element with children and properties. |
| `content-document` | Root content sequence. |

Each record has an optional `source` field. Element kinds are open symbols.
Children are proper lists of strings or content records; constructors turn
strings into `content-text` records. Elements may contain other elements,
text, and layout leaves. A `content-document` is a root sequence; nested
documents supplied to its constructor are flattened, and documents cannot be
element children.

Element properties are alists with unique symbol keys. Their values are
domain data, such as a link target or a math display flag. Unique keys keep
`content-element-property` and relation queries consistent: looking up a key
has one value. Property values are not interpreted by Grove.

IR records have no field setters and are intended to be immutable. Child lists,
property alists, and compound values inside them are ordinary Scheme data;
callers should not mutate them after construction or after a Forest has
indexed them.

Forester assigns query IDs to content elements when it builds a Forest. Text
and layout leaves contribute to ancestor text but do not have IDs of their
own. These integer IDs are traversal handles for that Forest snapshot, not
persistent identities. See
[Forester content queries](relations.md#forester-content-and-page-queries)
for their ordering and lifetime.

Grove provides generic rendering, per-kind handlers, and lazy handlers for
elements whose rendering chooses a different content context. Libraries such
as Forester define domain-specific meaning for their elements and analyze
them without adding those concepts to the Grove IR.

`(grove site)` is an optional page-oriented model. It groups pages by ID and
checks for duplicate IDs. A page contains an ID, title, content document,
optional source path, and an extensible metadata alist keyed by symbols. The
site contains no output paths or markup. The generic project pipeline does not
require this model; projects can return their own values from readers and
analyzers. Metadata keys are unique within a page so relation queries and
ordinary alist lookup agree on each value.
