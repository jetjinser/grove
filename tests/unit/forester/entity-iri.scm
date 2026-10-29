;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester entity-iri)
  #:use-module (srfi srfi-64)
  #:use-module (forester private entity-iri))

(test-begin "forester entity IRI")

(test-assert "accepts an HTTP IRI"
  (absolute-entity-iri? "https://example.org/entity/1"))
(test-assert "accepts a DOI IRI"
  (absolute-entity-iri? "doi:10.1000/182"))
(test-assert "accepts a scheme without an authority"
  (absolute-entity-iri? "mailto:reader@example.org"))
(test-assert "rejects relative references"
  (not (absolute-entity-iri? "../entity/1")))
(test-assert "accepts Unicode in an absolute HTTP IRI"
  (absolute-entity-iri? "https://例え.テスト/资料?q=é#章"))
(test-assert "accepts ASCII IDNA hostnames"
  (absolute-entity-iri? "https://xn--r8jz45g.xn--zckzah/"))
(test-assert "rejects malformed percent escapes"
  (not (absolute-entity-iri? "https://example.org/%Q0")))
(test-assert "rejects HTTP without a host"
  (not (absolute-entity-iri? "https:///entity/1")))
(test-assert "rejects userinfo"
  (not (absolute-entity-iri? "https://reader@example.org/entity/1")))

(test-assert "validates a distinct IRI list"
  (valid-entity-iri-list? '("doi:10.1000/182" "https://example.org/2")))
(test-assert "rejects duplicate entity IRIs"
  (not (valid-entity-iri-list? '("doi:10.1000/182" "doi:10.1000/182"))))

(test-equal "keeps web entity IRIs as browser links"
  "https://example.org/entity/1"
  (entity-iri-href "https://example.org/entity/1"))
(test-equal "preserves Unicode HTTP IRIs as browser links"
  "https://例え.テスト/资料?q=é#章"
  (entity-iri-href "https://例え.テスト/资料?q=é#章"))
(test-equal "resolves DOI entity IRIs through doi.org"
  "https://doi.org/10.1000/182"
  (entity-iri-href "doi:10.1000/182"))
(test-assert "does not treat other schemes as browser links"
  (not (entity-iri-href "mailto:reader@example.org")))
(test-assert "does not resolve DOI query variants"
  (not (entity-iri-href "doi:10.1000/182?source=grove")))

(test-end)
