;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester entity-uri)
  #:use-module (srfi srfi-64)
  #:use-module (forester private entity-uri))

(test-begin "forester entity URI")

(test-assert "accepts an HTTP URI"
  (absolute-entity-uri? "https://example.org/entity/1"))
(test-assert "accepts a DOI URI"
  (absolute-entity-uri? "doi:10.1000/182"))
(test-assert "accepts a scheme without an authority"
  (absolute-entity-uri? "mailto:reader@example.org"))
(test-assert "rejects relative references"
  (not (absolute-entity-uri? "../entity/1")))
(test-assert "rejects non-ASCII URI text"
  (not (absolute-entity-uri? "https://例え.テスト/資料")))
(test-assert "rejects malformed percent escapes"
  (not (absolute-entity-uri? "https://example.org/%Q0")))
(test-assert "rejects HTTP without a host"
  (not (absolute-entity-uri? "https:///entity/1")))
(test-assert "rejects userinfo"
  (not (absolute-entity-uri? "https://reader@example.org/entity/1")))

(test-assert "validates a distinct list"
  (valid-entity-uri-list? '("doi:10.1000/182" "https://example.org/2")))
(test-assert "rejects duplicate entity URIs"
  (not (valid-entity-uri-list? '("doi:10.1000/182" "doi:10.1000/182"))))

(test-equal "keeps web entity URIs as browser links"
  "https://example.org/entity/1"
  (entity-uri-href "https://example.org/entity/1"))
(test-equal "resolves DOI entity URIs through doi.org"
  "https://doi.org/10.1000/182"
  (entity-uri-href "doi:10.1000/182"))
(test-assert "does not treat other schemes as browser links"
  (not (entity-uri-href "mailto:reader@example.org")))
(test-assert "does not resolve DOI query variants"
  (not (entity-uri-href "doi:10.1000/182?source=grove")))

(test-end)
