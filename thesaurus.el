;;; thesaurus.el ---  Query thesaurus.com for synonyms of a given word -*- lexical-binding: t; -*-

;;; Copyright (C) 2022 by Anselm Coogan
;;; URL: https://github.com/AnselmC/thesaurus
;;; Version: 0.2.0
;;; Package-Requires: ((request "0.3.2") (emacs "24.4"))

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

(defun thesaurus--parse-synonyms-in-response (payload)
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

(defvar thesaurus--cache (make-hash-table :test 'equal))

(defun thesaurus--ask-thesaurus-for-synonyms (word)
  "Ask thesaurus.com for synonyms for WORD and return vector of synonyms."
  (progn
    (let ((cached-resp (gethash word thesaurus--cache)))
      (if cached-resp
          cached-resp
        (let* ((thesaurus-base-url "https://tuna.thesaurus.com/pageData/")
               (request-string (concat thesaurus-base-url word))
               (response (request-response-data (request request-string
                                                  :parser 'json-read
                                                  :sync t))))
          (if response
              (let ((synonyms (thesaurus--parse-synonyms-in-response response)))
                (progn 
                (puthash word synonyms thesaurus--cache)
                synonyms))
          (vector)))))))

;;;###autoload
(defun thesaurus-get-synonyms()
  "Interactively get synonyms for symbol at active region or point."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (replace-text (completing-read
                        (format "Select synonym for %S: " word)
                        (append (thesaurus--ask-thesaurus-for-synonyms word) '()))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert replace-text))))

(provide 'thesaurus)
;;; thesaurus.el ends here
