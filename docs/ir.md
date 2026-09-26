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

Each record has an optional `source` field. Element kinds are open.

`(grove site)` groups pages by ID and checks for duplicate IDs. A page
contains an ID, title, and content document. The site contains no output paths
or markup.
