#!/bin/sh
# SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Run axe-core in headless Chromium over built HTML pages.
#
# Usage: scripts/a11y-audit.sh [HTML ...]
# With no arguments, audits the built example pages below.
# Set CHROMIUM to override browser discovery.  The first run downloads
# axe-core (pinned and checksum-verified) into the user cache.

set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
axe_version=4.10.2
axe_sha256=b511cd9dec01c76f4b2ad1723b66b6db37d4c2eb4ed199076e1829d9ee7b75e3
cache_dir=${XDG_CACHE_HOME:-$HOME/.cache}/grove
axe=$cache_dir/axe-core-$axe_version.js

find_chromium() {
    if [ -n "${CHROMIUM:-}" ]; then
        printf '%s\n' "$CHROMIUM"
        return
    fi
    for command in chromium chromium-browser google-chrome; do
        if command -v "$command" >/dev/null 2>&1; then
            command -v "$command"
            return
        fi
    done
    for path in /gnu/store/*-chromium-*/bin/chromium \
                /gnu/store/*-ungoogled-chromium-*/bin/chromium; do
        if [ -x "$path" ]; then
            printf '%s\n' "$path"
            return
        fi
    done
    return 1
}

ensure_axe() {
    if [ -f "$axe" ] &&
       printf '%s  %s\n' "$axe_sha256" "$axe" | sha256sum -c - >/dev/null 2>&1; then
        return
    fi
    mkdir -p "$cache_dir"
    temporary=$axe.tmp.$$
    if command -v curl >/dev/null 2>&1; then
        curl -fsSL "https://unpkg.com/axe-core@$axe_version/axe.min.js" -o "$temporary"
    elif command -v wget >/dev/null 2>&1; then
        wget -q "https://unpkg.com/axe-core@$axe_version/axe.min.js" -O "$temporary"
    else
        echo "a11y-audit: curl or wget is required to download axe-core" >&2
        exit 1
    fi
    if ! printf '%s  %s\n' "$axe_sha256" "$temporary" |
         sha256sum -c - >/dev/null 2>&1; then
        rm -f "$temporary"
        echo "a11y-audit: axe-core checksum mismatch" >&2
        exit 1
    fi
    mv "$temporary" "$axe"
}

chromium=$(find_chromium) || {
    echo "a11y-audit: no Chromium found; set CHROMIUM=/path/to/chromium" >&2
    exit 1
}
ensure_axe

if [ "$#" -eq 0 ]; then
    set -- \
        "$root/examples/bilingual/build/index.html" \
        "$root/examples/linked-notes/build/index.html" \
        "$root/examples/showcases/heat-conduction/build/index.html" \
        "$root/examples/showcases/forest-category/build/index.html" \
        "$root/examples/showcases/math-derivations/build/index.html"
fi

temporary=$(mktemp -d)
trap 'rm -rf "$temporary"' EXIT HUP INT TERM

cat > "$temporary/inject.html" <<HTML
<script src="file://$axe"></script>
<script>
window.addEventListener("load", function () {
  axe.run(document, {runOnly: {type: "tag", values: ["wcag2a", "wcag2aa", "wcag21a", "wcag21aa"]}})
    .then(function (results) {
      var pre = document.createElement("pre");
      pre.id = "axe-results";
      pre.textContent = results.violations.length
        ? results.violations.map(function (violation) {
            return violation.id + " [" + violation.impact + "] " + violation.help + "\n" +
              violation.nodes.map(function (node) {
                return "  " + node.target.join(" ") + " :: " + node.html;
              }).join("\n");
          }).join("\n")
        : "NO VIOLATIONS";
      document.body.appendChild(pre);
    })
    .catch(function (error) {
      var pre = document.createElement("pre");
      pre.id = "axe-results";
      pre.textContent = "AXE ERROR: " + error;
      document.body.appendChild(pre);
    });
});
</script>
HTML

status=0
index=0
for page in "$@"; do
    index=$((index + 1))
    if [ ! -f "$page" ]; then
        echo "a11y-audit: missing page: $page" >&2
        status=1
        continue
    fi
    copy=$temporary/page-$index.html
    dom=$temporary/page-$index.dom
    cat "$page" "$temporary/inject.html" > "$copy"
    "$chromium" --headless=new --no-sandbox --disable-gpu \
        --allow-file-access-from-files --virtual-time-budget=15000 \
        --dump-dom "file://$copy" > "$dom" 2>/dev/null || true
    if ! grep -q 'id="axe-results"' "$dom"; then
        echo "a11y-audit: no axe result for $page" >&2
        status=1
        continue
    fi
    result=$(awk '
        /id="axe-results"/ { active = 1; sub(/.*<pre id="axe-results">/, "") }
        active { print }
        /<\/pre>/ { active = 0 }
    ' "$dom" | sed 's|</pre>.*||; s|&lt;|<|g; s|&gt;|>|g; s|&amp;|\&|g')
    if [ "$result" = "NO VIOLATIONS" ]; then
        echo "ok: $page"
    else
        echo "violations: $page" >&2
        printf '%s\n' "$result" >&2
        status=1
    fi
done
exit "$status"
