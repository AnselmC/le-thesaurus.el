;;; test-le-thesaurus.el ---  Tests for le-thesaurus.el -*- lexical-binding: t; -*-

;;; Copyright (C) 2022 by Anselm Coogan
;;; URL: https://github.com/AnselmC/le-thesaurus
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

;;; This file contains tests for le-thesaurus.el

;;; Code:

(when (require 'undercover nil t)
  (undercover "*.el"
	          (:report-format 'lcov)
	          (:send-report nil)))

(require 'buttercup)
(require 'le-thesaurus)
(require 'benchmark)

(describe "le-thesaurus correctly extracts data from parsed JSON response."
          (it "Returns an empty list when the payload has no 'data key empty"
              (let ((payload '()))
                (dolist (type '(antonyms synonyms))
                  (expect (le-thesaurus--parse-data-in-response payload type) :to-be '()))))
          (it "Returns an empty list when the payload's 'data value has no 'defintionData key"
              (let ((payload '((data . ()))))
                (dolist (type '(antonyms synonyms))
                  (expect (le-thesaurus--parse-data-in-response payload type) :to-be '()))))
          (it "Returns an empty list when the payload's 'definitionData has no 'definitions key"
              (let ((payload '((data . ((definitionData . ()))))))
                (dolist (type '(antonyms synonyms))
                  (expect (le-thesaurus--parse-data-in-response payload type) :to-be '()))))
          (it "Returns an empty list when the `definitions list is empty"
              (let ((payload '((data . ((definitionData . ((definitions . ()))))))))
                (dolist (type '(antonyms synonyms))
                  (expect (le-thesaurus--parse-data-in-response payload type) :to-be '()))))
          (it "Returns the synonyms and antonyms on simple payload"
              (let ((payload '((data .
                                     ((definitionData .
                                        ((definitions .
                                           ((
                                             (synonyms .
                                                       (((term . "hello") (similarity . "100") (isVulgar . "0") (isInformal . "0"))
                                                        ((term . "world") (similarity . "42") (isVulgar . "100") (isInformal . "100"))))
                                             (antonyms .
                                                       (((term . "bye") (similarity . "10") (isVulgar . "0") (isInformal . "0"))
                                                        ((term . "girl") (similarity . "24") (isVulgar . "100") (isInformal . "100"))))))))))))))
                (expect (le-thesaurus--parse-data-in-response payload 'synonyms) :to-equal
                        '(((similarity . "100") (vulgar) (informal) (definition) (term . "hello"))
                          ((similarity . "42") (vulgar . t) (informal . t) (definition) (term . "world"))))
                (expect (le-thesaurus--parse-data-in-response payload 'antonyms) :to-equal
                        '(((similarity . "10") (vulgar) (informal) (definition) (term . "bye"))
                          ((similarity . "24") (vulgar . t) (informal . t) (definition) (term . "girl"))))
                ))
          (it "Returns the synonyms from full example json payload"
              (let ((payload (json-read-file "tests/data/test-response-synonyms.json")))
                (expect (le-thesaurus--parse-data-in-response payload 'synonyms) :to-equal
                        '(((similarity . "100") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "reference book"))
                          ((similarity . "50") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "glossary"))
                          ((similarity . "50") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "lexicon"))
                          ((similarity . "50") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "onomasticon"))
                          ((similarity . "50") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "terminology"))
                          ((similarity . "50") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "vocabulary"))
                          ((similarity . "10") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "language reference book"))
                          ((similarity . "10") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "sourcebook"))
                          ((similarity . "10") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "storehouse of words"))
                          ((similarity . "10") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "treasury of words"))
                          ((similarity . "10") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "word list"))))))
          (it "Returns the antonmys from full example json payload"
              (let ((payload (json-read-file "tests/data/test-response-antonyms.json")))
                (expect (le-thesaurus--parse-data-in-response payload 'antonyms) :to-equal
                        '(((similarity . "-100") (vulgar) (informal) (definition . "horrible, frightening") (term . "advantageous"))
                          ((similarity . "-100") (vulgar) (informal) (definition . "horrible, frightening") (term . "beautiful"))
                          ((similarity . "-100") (vulgar) (informal) (definition . "horrible, frightening") (term . "comforting")))))))

(xdescribe "le-thesaurus correctly works with thesaurus.com."
          (it "Returns the expected list of synonyms for 'thesaurus'"
              (defvar auto-revert-notify-watch-descriptor-hash-list nil);; there's a bug in request it seems s.t. I need to define this var
              (let ((word "thesaurus"))
                (expect (le-thesaurus--ask-thesaurus-for-word word 'synonyms) :to-equal
                        '(((similarity . "100") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "reference book"))
                          ((similarity . "50") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "glossary"))
                          ((similarity . "50") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "lexicon"))
                          ((similarity . "50") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "onomasticon"))
                          ((similarity . "50") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "terminology"))
                          ((similarity . "50") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "vocabulary"))
                          ((similarity . "10") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "language reference book"))
                          ((similarity . "10") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "sourcebook"))
                          ((similarity . "10") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "storehouse of words"))
                          ((similarity . "10") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "treasury of words"))
                          ((similarity . "10") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "word list"))))))
          (it "Sets the cache when a word is requested"
              (let* ((word "dictionary")
                     (synonyms (le-thesaurus--ask-thesaurus-for-word word 'synonyms)))
                (expect (le-thesaurus--parse-data-in-response (gethash word le-thesaurus--cache) 'synonyms) :to-equal synonyms)))
          (it "Uses the cache when the same word is requested twice"
              (let* ((word "ball")
                     (first-run-secs (benchmark-elapse (le-thesaurus--ask-thesaurus-for-word word 'synonyms)))
                     (second-run-secs (benchmark-elapse (le-thesaurus--ask-thesaurus-for-word word 'synonyms))))
                ;; using factor of 5 to make sure that performance increase isn't random variation in network response time
                (expect first-run-secs :to-be-greater-than (* 5 second-run-secs)))))

(describe "le-thesaurus correctly provides completions"
          :var ((synonyms '(((similarity . "50") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "vocabulary"))
                            ((similarity . "100") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "reference book"))
                            ((similarity . "10") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "language reference book"))
                            ((similarity . "20") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "word list")))))
          (it "Creates an alist where the key is the synonym, the value contains its metadata, and it's sorted by similarity"
              (let ((completions (le-thesaurus--get-completions synonyms)))
                (expect completions :to-equal
                        '(("reference book" (similarity . "100") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "reference book"))
                          ("vocabulary" (similarity . "50") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "vocabulary"))
                          ("word list" (similarity . "20") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "word list"))
                          ("language reference book" (similarity . "10") (vulgar) (informal) (definition . "dictionary of synonyms and antonyms") (term . "language reference book"))))))
          (it "Creates annotations containing definitions and similarities of each synonym"
              (let* ((word "word list")
                     (annotation (funcall (le-thesaurus--get-annotations (le-thesaurus--get-completions synonyms)) word)))
                (expect annotation :to-equal (format "\s\s\s\s\s\s\s\s\s\s\s\s\s\s\s\sSim:  20\tDef: dictionary of synonyms and antonyms\t\t"))))
          (it "Correctly uses the definition for the group of a synonym"
              (let* ((completions (le-thesaurus--get-completions synonyms))
                     (word "reference book")
                     (group (funcall (le-thesaurus--get-group completions) word '()))
                     (completion (funcall (le-thesaurus--get-group completions) word t)))
                (expect group :to-equal "dictionary of synonyms and antonyms")
                (expect completion :to-equal word))))


(provide 'test-le-thesaurus)
;;; test-le-thesaurus.el ends here
