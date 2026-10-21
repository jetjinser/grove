#!/bin/sh
# SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# The full local check: test suite, license audit, example builds, and axe.
# Run inside the development shell.

set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$root"

blue check
reuse --no-multiprocessing lint
"$root/scripts/check-examples.sh"
