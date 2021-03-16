;;; package ---  Query thesaurus.com for synonyms of a given word.

;;; Copyright (C) 2021 by Anselm Coogan
;;; URL: https://github.com/AnselmC/thesaurus
;;; Version: 0.1

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

;;; Uses the Elisp request library (https://github.com/tkf/emacs-request) and the thesaurus.com API to fetch synonyms

;;; Code:

(require 'cl-lib)
(require 'request)

(defun parse-synonyms-in-response (payload)
  "Parse JSON PAYLOAD to extract synonyms from response."
  (let* ((data (assoc-default 'data payload))
         (definition-data (if data
                              (assoc-default 'definitionData data)
                            '()))
         (definitions-list (if definition-data
                               (assoc-default 'definitions definition-data)
                             '()))
         (definitions (if definitions-list
                          (aref definitions-list 0)
                        '()))
         (synonyms (if definitions
                       (cl-map 'vector
                               #'(lambda (e) (assoc-default 'term e))
                               (assoc-default 'synonyms definitions))
                     (vector))))
    synonyms))

(defun ask-thesaurus-for-synonyms (word)
  "Ask thesaurus.com for synonyms for WORD and return vector of synonyms (possibly empty)."
  (let* ((thesaurus-base-url "https://tuna.thesaurus.com/pageData/")
         (request-string (concat thesaurus-base-url word))
         (response (request-response-data (request request-string
                                            :parser 'json-read
                                            :sync t))))
    (if response
        (parse-synonyms-in-response response)
      (vector))))

(defun get-synonyms()
  "Interactively get synonyms for symbol at point."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (replace-text (completing-read
                        (format "Select synonym for %S: " word)
                        (append (ask-thesaurus-for-synonyms word) '()))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert replace-text))))

(provide 'thesaurus)
;;; thesaurus.el ends here
