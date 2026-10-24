<!-- SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu> -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

# External asset experiment

This project-side experiment feeds a standalone Graphviz file through Grove's
asset pipeline. The adapter is [graphviz.scm](graphviz.scm), and the input is
[diagram.dot](diagram.dot). Its integration test checks real SVG output,
input-driven rebuilding, and reproducibility.

The pipeline cache includes its declared `#:version`, but does not inspect the
executable found through `PATH`. Replacing Graphviz while keeping the same
version key reuses the old SVG; pin the tool in the project environment or
change the key when the tool or rendering options change. The integration test
also exercises this boundary with two stand-in `dot` executables.
