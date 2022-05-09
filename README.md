# le-thesaurus.el
[![MELPA](https://melpa.org/packages/le-thesaurus-badge.svg)](https://melpa.org/#/le-thesaurus)
[![Coverage Status](https://coveralls.io/repos/github/AnselmC/le-thesaurus.el/badge.svg?branch=master)](https://coveralls.io/github/AnselmC/le-thesaurus.el?branch=master)

Emacs package for querying synonyms from [Thesaurus.com](thesaurus.com).

<p align="center">
    <img src="thesaurus-example.gif"/>
</p>

## Installation
le-thesaurus is available via [MELPA](https://melpa.org/).

Install it via [use-package](https://github.com/jwiegley/use-package) or [straight](https://github.com/raxod502/straight.el)
```elisp
(use-package le-thesaurus)
```

## Usage
Put your cursor on the word you'd like a synonym for and run `M-x le-thesaurus-get-synonyms`.
Pick a synonym from the completion buffer and hit `RET`.

Responses from thesaurus.com are cached. 
You can clear the cache with `le-thesaurus-clear-cache` if it should get too large.

That's it.


## Development
Pull requests are welcome.

### Tests
Tests use the [buttercup framework](https://github.com/jorgenschaefer/emacs-buttercup).
You can run tests via

``` shell
make test
```
