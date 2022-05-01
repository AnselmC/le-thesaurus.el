;;; test-thesaurus.el ---  Tests for thesaurus.el -*- lexical-binding: t; -*-

;;; Copyright (C) 2022 by Anselm Coogan
;;; URL: https://github.com/AnselmC/thesaurus
;;; Version: 0.2.0
;;; Package-Requires: ((buttercup "1.25") (emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; This file contains tests for thesaurus.el

;;; Code:

(when (require 'undercover nil t)
  (undercover "*.el"
	          (:report-format 'lcov)
	          (:send-report nil)))

(require 'buttercup)
(require 'thesaurus)
(require 'benchmark)

(describe "thesaurus correctly extracts synonyms from parsed JSON response."
  (it "Returns an empty vector when the payload has no 'data key empty"
    (let ((payload '()))
      (expect (thesaurus--parse-synonyms-in-response payload) :to-be '[])))
  (it "Returns an empty vector when the payload's 'data value has no 'defintionData key"
    (let ((payload '((data . ()))))
      (expect (thesaurus--parse-synonyms-in-response payload) :to-be '[])))
  (it "Returns an empty vector when the payload's 'definitionData has no 'definitions key"
    (let ((payload '((data . ((definitionData . ()))))))
      (expect (thesaurus--parse-synonyms-in-response payload) :to-be '[])))
  (it "Returns an empty vector when the `definitions list is empty"
    (let ((payload '((data . ((definitionData . ((definitions . ()))))))))
      (expect (thesaurus--parse-synonyms-in-response payload) :to-be '[])))
  (it "Returns the synonyms on simple payload"
    (let ((payload '((data . ((definitionData . ((definitions . [((synonyms . [((term . "hello")) ((term . "world"))]))]))))))))
      (expect (thesaurus--parse-synonyms-in-response payload) :to-equal '["hello" "world"])))
  (it "Returns the synonyms from full example json payload"
    (let ((payload (json-read-file "tests/data/test-response.json")))
      (expect (thesaurus--parse-synonyms-in-response payload) :to-equal '["reference book" "glossary" "lexicon" "onomasticon" "terminology" "vocabulary" "language reference book" "sourcebook" "storehouse of words" "treasury of words" "word list"]))))

(describe "thesaurus correctly works with thesaurus.com."
  (it "Returns the expected list of synonyms for 'thesaurus'"
    ;; there's a bug in request it seem s.t. I need to define this var
    (defvar auto-revert-notify-watch-descriptor-hash-list nil)
    (let ((word "thesaurus")
          (expected-synonyms '["reference book" "glossary" "lexicon" "onomasticon" "terminology" "vocabulary" "language reference book" "sourcebook" "storehouse of words" "treasury of words" "word list"]))
      (expect (thesaurus--ask-thesaurus-for-synonyms word) :to-equal expected-synonyms)))
  (it "Sets the cache when a word is requested"
    (let* ((word "dictionary")
           (synonyms (thesaurus--ask-thesaurus-for-synonyms word)))
      (expect (gethash word thesaurus--cache) :to-be synonyms)))
  (it "Uses the cache when the same word is requested twice"
    (let* ((word "ball")
            (first-run-secs (benchmark-elapse (thesaurus--ask-thesaurus-for-synonyms word)))
            (second-run-secs (benchmark-elapse (thesaurus--ask-thesaurus-for-synonyms word))))
         ;; using factor of 5 to make sure that performance increase isn't random variation in network response time
         (expect first-run-secs :to-be-greater-than (* 5 second-run-secs)))))

(provide 'test-thesaurus)
;;; test-thesaurus.el ends here
