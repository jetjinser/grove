;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;; SPDX-License-Identifier: MIT

(use-modules (srfi :275 path))

(unless (and (path? (string->path "a/b"))
             (not (defined? 'uri?)))
  (error "Guile-native path module contract failed"))
