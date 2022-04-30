(require 'thesaurus)

(describe "thesaurus correctly extracts synonyms from parsed JSON response."
  (it "Returns an empty vector when the payload has no 'data key empty"
    (let ((payload '()))
    (expect (thesaurus-parse-synonyms-in-response payload) :to-be '[])))
  (it "Returns an empty vector when the payload's 'data value has no 'defintionData key"
    (let ((payload '((data . ()))))
    (expect (thesaurus-parse-synonyms-in-response payload) :to-be '[])))
  (it "Returns an empty vector when the payload's 'definitionData has no 'definitions key"
    (let ((payload '((data . ((definitionData . ()))))))
    (expect (thesaurus-parse-synonyms-in-response payload) :to-be '[])))
  (it "Returns an empty vector when the `definitions list is empty"
    (let ((payload '((data . ((definitionData . ((definitions . ()))))))))
    (expect (thesaurus-parse-synonyms-in-response payload) :to-be '[])))
  (it "Returns the synonyms on simple payload"
    (let ((payload '((data . ((definitionData . ((definitions . [((synonyms . [((term . "hello")) ((term . "world"))]))]))))))))
    (expect (thesaurus-parse-synonyms-in-response payload) :to-equal '["hello" "world"]))
    )
  (it "Returns the synonyms from full example json payload"
    (let ((payload (json-read-file "test-response.json")))
    (expect (thesaurus-parse-synonyms-in-response payload) :to-equal '["reference book" "glossary" "lexicon" "onomasticon" "terminology" "vocabulary" "language reference book" "sourcebook" "storehouse of words" "treasury of words" "word list"]))))

(describe "thesaurus correctly works with thesaurus.com."
  (it "Returns the expected list of synonyms for 'thesaurus'"
    ;; there's a bug in request it seems
    (setq auto-revert-notify-watch-descriptor-hash-list nil)
    (let ((word "thesaurus")
          (expected-synonyms '["reference book" "glossary" "lexicon" "onomasticon" "terminology" "vocabulary" "language reference book" "sourcebook" "storehouse of words" "treasury of words" "word list"]))
      (expect (thesaurus-ask-thesaurus-for-synonyms word) :to-equal expected-synonyms))))
