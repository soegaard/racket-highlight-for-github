racket-highlight-for-github
===========================

Github syntax highlighting for Racket 

- nothing here yet


Resources:

As it turns out, SublimeText has a readable introduction for writing TextMate lexers:
    http://docs.sublimetext.info/en/latest/extensibility/syntaxdefs.html#scopes-and-scope-selectors

To develop a lexer:
  - install the text editor Sublime Editor 3
  - install https://github.com/SublimeText/AAAPackageDev
    (Just make copy of AAAPackageDev in the folder described in syntaxdefs.html)
  - After editing the Racket definitions hit F7 and change to tab with Racket file to see the results.
  - Place curson on top of interesting syntax in a rkt file.
    cmd+alt+P  will show the detected scope (type) in the status line at the bottom.
