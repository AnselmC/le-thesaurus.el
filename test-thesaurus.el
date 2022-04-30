(require 'thesaurus)

(describe "thesaurus correctly extracts synonyms from parsed JSON response."
  :var (payload)
  (it "Returns an empty vector when the payload has no 'data key empty"
    (setq payload '())
    (expect (thesaurus-parse-synonyms-in-response payload) :to-be '[]))
  (it "Returns an empty vector when the payload's 'data value has no 'defintionData key"
    (setq payload '((data . ())))
    (expect (thesaurus-parse-synonyms-in-response payload) :to-be '[]))
  (it "Returns an empty vector when the payload's 'definitionData has no 'definitions key"
    (setq payload '((data . ((definitionData . ())))))
    (expect (thesaurus-parse-synonyms-in-response payload) :to-be '[]))
  (it "Returns an empty vector when the `definitions list is empty"
    (setq payload '((data . ((definitionData . ((definitions . ())))))))
    (expect (thesaurus-parse-synonyms-in-response payload) :to-be '[]))
  (it "Returns the synonyms on simple payload"
    (setq payload '((data . ((definitionData . ((definitions . [((synonyms . [((term . "hello")) ((term . "world"))]))])))))))
    (expect (thesaurus-parse-synonyms-in-response payload) :to-equal '["hello" "world"])
    )
  (it "Returns the synonyms from full example json payload"
    (setq payload (json-read-file "test-response.json"))
    (expect (thesaurus-parse-synonyms-in-response payload) :to-equal '["reference book" "glossary" "lexicon" "onomasticon" "terminology" "vocabulary" "language reference book" "sourcebook" "storehouse of words" "treasury of words" "word list"])))

(describe "thesaurus correctly works with thesaurus.com."
  (it "Returns the expected list of synonyms for 'thesaurus'"
    (let ((word "thesaurus")
          (expected-synonyms '["reference book" "glossary" "lexicon" "onomasticon" "terminology" "vocabulary" "language reference book" "sourcebook" "storehouse of words" "treasury of words" "word list"]))
      (expect (thesaurus-ask-thesaurus-for-synonyms word) :to-equal expected-synonyms))))
