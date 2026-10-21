#!/bin/sh
# SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Build the examples covered by the accessibility audit.
# Run inside the development shell (blue must be on PATH).

set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$root"

for project in \
    examples/bilingual/project.scm \
    examples/linked-notes/project.scm \
    examples/showcases/heat-conduction/project.scm \
    examples/showcases/forest-category/project.scm \
    examples/showcases/math-derivations/project.scm; do
    echo "building $project"
    blue grove build --project "$project"
done

exec "$root/scripts/a11y-audit.sh"
