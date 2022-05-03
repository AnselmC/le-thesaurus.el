;;; le-thesaurus.el ---  Query thesaurus.com for synonyms of a given word -*- lexical-binding: t; -*-

;;; Copyright (C) 2022 by Anselm Coogan
;;; URL: https://github.com/AnselmC/le-thesaurus
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

(defun le-thesaurus--parse-synonyms-in-response (payload)
  "Parse JSON PAYLOAD to extract synonyms from response."
  (let* ((data (assoc-default 'data payload))
         (definition-data (if data
                              (assoc-default 'definitionData data)
                            '()))
         (definitions-list (if definition-data
                               (assoc-default 'definitions definition-data)
                             '())))
    (if definitions-list
        (aref definitions-list 0)
      '())))


(defun le-thesaurus--get-completions (word-data)
  (defun put-into-hash-table (e hash-t)
    (let ((synonym (assoc-default 'term e)))
      (puthash synonym e hash-t)))

  (let* ((synonyms-data (assoc-default 'synonyms word-data))
         (hash-table (make-hash-table :test 'equal)))
    (progn
      (mapc #'(lambda (e) (put-into-hash-table e hash-table)) synonyms-data)
      hash-table)))




(defun le-thesaurus--get-completions (synonyms)
  (cl-map 'vector
          #'(lambda (e) (assoc-default 'term e))
          synonyms))

(defun le-thesaurus--get-annotations (word)
  (let ((metadata (gethash word minibuffer-completion-table)))
    (concat "\tSimilarity: " (assoc-default 'similarity metadata)
            "\tInformal: " (if (equal (assoc-default 'isInformal metadata) "0") "No" "Yes")
            "\tVulgar: " (if (equal (assoc-default 'isVulgar metadata) "0") "No" "Yes"))))


(defvar le-thesaurus--cache (make-hash-table :test 'equal))

(defun le-thesaurus--ask-thesaurus-for-synonyms (word)
  "Ask thesaurus.com for synonyms for WORD and return vector of synonyms."
  (progn
    (let ((cached-resp (gethash word le-thesaurus--cache)))
      (if cached-resp
          cached-resp
        (let* ((thesaurus-base-url "https://tuna.thesaurus.com/pageData/")
               (request-string (concat thesaurus-base-url word))
               (response (request-response-data (request request-string
                                                  :parser 'json-read
                                                  :sync t))))
          (if response
              (let ((synonyms (le-thesaurus--parse-synonyms-in-response response)))
                (progn
                  (puthash word synonyms le-thesaurus--cache)
                  synonyms))
            (vector)))))))

;;;###autoload
(defun le-thesaurus-get-synonyms()
  "Interactively get synonyms for symbol at active region or point."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (results (le-thesaurus--ask-thesaurus-for-synonyms word))
         (definition (assoc-default 'definition results))
         (completions (le-thesaurus--get-completions results))
         (completion-extra-properties '(:annotation-function le-thesaurus--get-annotations))
         (replace-text (completing-read
                        (format "Select synonym for %S: (%s)" word definition)
                        completions)))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert replace-text))))

(provide 'le-thesaurus)
;;; le-thesaurus.el ends here
