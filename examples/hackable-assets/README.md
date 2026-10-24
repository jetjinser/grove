<!-- SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu> -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

# External asset experiment

This project-side experiment feeds a standalone Graphviz file through Grove's
asset pipeline. The adapter is [graphviz.scm](graphviz.scm), and the input is
[diagram.dot](diagram.dot). Its integration test checks real SVG output,
input-driven rebuilding, and reproducibility.

The adapter probes `dot -V` and includes the result in its cache identity, so
project code does not need to repeat Graphviz's version. The project environment
still needs to pin the tool if reproducible builds matter; Grove cannot infer
whether a tool's reported version captures every behavior-affecting dependency.
The integration test replaces a stand-in `dot` with a different reported
version and verifies automatic cache invalidation.
