
"if has('termguicolors')
"  set termguicolors
"endif
" Set the background color. Available values: dark and light
set background=dark
" Set contrast. Available values: 'hard', 'medium'(default), 'soft'
let g:everforest_background = 'soft'
" For better performance
let g:everforest_better_performance = 1
colorscheme everforest

syntax enable

set autoindent " inherit indentation from the previous line
set expandtab " convert tabs into consecutive spaces
set tabstop=2 " one tab is equal to 2 consecutive spaces
set softtabstop=2 " the same for soft tabs
set shiftwidth=2 " when shifting, indent using 2 spaces

set noerrorbells " disable beeps
set number " show line numbers on the sidebar.
set cursorline " highlight current line
set mouse=a
set encoding=utf-8
set spell spelllang=en_gb

set backspace=indent,eol,start

set noswapfile
set incsearch " highlight text as you search within it with /
set scrolloff=0

"set textwidth=75
set colorcolumn=76
set equalalways
"set splitright

