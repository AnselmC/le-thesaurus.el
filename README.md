# le-thesaurus.el
[![Coverage Status](https://coveralls.io/repos/github/AnselmC/le-thesaurus.el/badge.svg?branch=master)](https://coveralls.io/github/AnselmC/le-thesaurus.el?branch=master)

Emacs package for querying synonyms from [Thesaurus.com](thesaurus.com).

<p align="center">
    <img src="thesaurus-example.gif"/>
</p>

## Installation
Clone this repo and add it to your load path:
```elisp
(add-to-list 'load-path "/path/to/this/repo")
```

## Usage
Put your cursor on the word you'd like a synonym for and run `M-x le-thesaurus-get-synonyms`.
Pick a synonym from the completion buffer and hit `RET`.
That's it.


## Development
Pull requests are welcome.

### Tests
Tests use the [buttercup framework](https://github.com/jorgenschaefer/emacs-buttercup).
You can run tests via

``` shell
make test
```
