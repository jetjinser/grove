<!-- SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu> -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

# External asset experiment

This project-side experiment feeds a standalone Graphviz file through Grove's
asset pipeline. The adapter is [graphviz.scm](graphviz.scm), and the input is
[diagram.dot](diagram.dot). Its integration test checks real SVG output,
input-driven rebuilding, and reproducibility.
