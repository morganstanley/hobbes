" Vim syntax for hobbes expressions

if exists("b:current_syntax")
  finish
endif

" keywords
syn keyword hKeywords true false
syn keyword hKeywords bool byte char short int long float double time datetime timespan
syn keyword hKeywords option module import where exists
syn keyword hKeywords data type class instance
syn keyword hKeywords pack unpack
syn keyword hKeywords if then else
syn keyword hKeywords and or
syn keyword hKeywords case of
syn keyword hKeywords let in
syn keyword hKeywords match matches with
syn keyword hKeywords do return
syn keyword hKeywords _

" constants
syn match hNum '\<\d\+\>'
syn match hNum '\<\d\+S\>'
syn match hNum '\<\d\+L\>'
syn match hNum '\<\d\+\.\d\+f\=\>'

syn region hString start='"' skip='\\.' end='"'

syn match hChar "'.'"
syn match hChar "'\\.'"

syn match hByte '0X[0-9a-fA-F]\{2}'
syn match hBytes '0x[0-9a-fA-F]\+'

" comments
syn match hComment '//.*$'
syn region hBlockComment start='/\*' end='\*/'

" assignment
syn match hAssign '<-'

" color highlighting
let b:current_syntax = "hobbes"

hi Danger ctermfg=218 guifg=#ffafd7

hi def link hComment      Comment
hi def link hBlockComment Comment
hi def link hString       String
hi def link hNum          Constant
hi def link hChar         Constant
hi def link hByte         Constant
hi def link hBytes        Constant
hi def link hKeywords     Keyword
hi def link hAssign       Danger

