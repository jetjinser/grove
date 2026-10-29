;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;; SPDX-License-Identifier: MIT

(define-module (srfi :275 path)
  #:use-module (srfi :275 internal path)
  #:re-export (path? path-segments relative-path empty-path absolute-path
               empty-absolute-path path=? path-length path-ref path-update
               relative-path? absolute-path? empty-path?
               rfc3987-path-abempty? rfc3987-path-absolute?
               rfc3987-path-rootless? rfc3987-path-noscheme?
               path-string? path->string string->path
               string->absolute-path string->relative-path
               list->absolute-path list->relative-path remove-dot-segments
               merge-paths build-path char-set:path))
