<!-- SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu> -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

# External asset experiment

This project-side experiment feeds a standalone Graphviz file through Grove's
asset pipeline. The adapter is [graphviz.scm](graphviz.scm), and the input is
[diagram.dot](diagram.dot). Its integration test checks real SVG output,
input-driven rebuilding, and reproducibility.

Grove fingerprints project code imported at load time, including the adapter.
The adapter probes `dot -V` and includes it in the cache identity because the
external executable is outside that load graph. The project environment still
needs to pin the tool if reproducible builds matter; Grove cannot infer whether
a reported version captures every behavior-affecting dependency. The adapter's
`dot-svg-v1` prefix is its own behavior version; bump it when the conversion
logic changes in ways the source fingerprint does not capture.
The integration test replaces a stand-in `dot` with a different reported
version and verifies automatic cache invalidation.
