<!-- SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu> -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

# Hackable math experiment

This project-side experiment checks whether Grove's extension points can carry
Typst, LaTeX, and SMathML source in `.plant` articles. It is not a Grove core
API. The sample is [math.plant](math.plant); the implementation is
[extension.scm](extension.scm); executable checks are in
[`tests/unit/forester/hackable-math.scm`](../../tests/unit/forester/hackable-math.scm).

The unit test uses stand-in executables and does not need the external tools.
To run the real renderers, enter Grove's optional Guix environment with
`guix shell -m manifest.scm -m integration-manifest.scm` from the repository
root, or use `nix develop .#integrations`. These provide Typst, Racket with
SMathML, and a minimal LaTeX kernel plus `amsmath` and `dvisvgm`.
