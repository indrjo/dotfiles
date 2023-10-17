
" Disable compatibility with vi which can cause unexpected issues.
set nocompatible

" Enable type file detection.
filetype on

" Enable plugins and load plugin for the detected file type.
filetype plugin on

" Turn syntax highlighting on.
syntax on

" Disable bips
set noerrorbells

" Add numbers to each line on the left-hand side.
set number

" Highlight cursor line underneath the cursor horizontally.
set cursorline

" Highlight cursor line underneath the cursor vertically.
"set cursorcolumn

" Indentation
filetype indent on
set autoindent    " inherit indentation from the previous line
set expandtab     " convert tabs into consecutive spaces
set tabstop=2     " one tab is equal to 2 consecutive spaces
set softtabstop=2 " the same for soft tabs
set shiftwidth=2  " when shifting, indent using 2 spaces

" How many columns per line.
set textwidth=75
set colorcolumn=76

" Do not save backup files.
set nobackup

" Do not let cursor scroll below or above N number of lines when scrolling.
set scrolloff=10

" Do not wrap lines. Allow long lines to extend as far as the line goes.
set nowrap

" While searching though a file, highlight matching characters as you type.
set incsearch

" Ignore capital letters during search.
set ignorecase

" Override the ignorecase option if searching for capital letters.
" This will allow you to search specifically for capital letters.
set smartcase

" Show partial command you type in the last line of the screen.
set showcmd

" Show the mode you are on the last line.
set showmode

" Show matching words during a search.
set showmatch

" Use highlighting when doing a search.
set hlsearch

" Set the commands to save in history default number is 20.
set history=1000

" Enable auto completion menu after pressing TAB.
set wildmenu

" Make wildmenu behave like similar to Bash completion.
set wildmode=list:longest

" There are certain files that we would never want to edit with Vim.
" Wildmenu will ignore files with these extensions.
set wildignore=*.docx,*.jpg,*.png,*.gif,*.pdf,*.pyc,*.exe,*.flv,*.img,*.xlsx

" Use mouse/touchpad too.
set mouse=a

" Set the background color. Available values: dark and light.
set background=dark
" Set contrast. Available values: 'hard', 'medium'(default), 'soft'
"let g:everforest_background = 'soft'
" For better performance
let g:everforest_better_performance = 1
colorscheme everforest

" Plugins for vim.
call plug#begin('~/.vim/plugged')

" TeX and LaTeX
Plug 'lervag/vimtex'
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
"set conceallevel=1
"let g:tex_conceal='abdmg'

" A Vim Plugin for Lively Previewing LaTeX PDF Output
"Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }
"let g:livepreview_previewer = 'evince'
"let g:livepreview_engine = 'lualatex'
"let g:livepreview_texinputs = '~/texlive//:~/.texlive//'

" Track the engine.
Plug 'SirVer/ultisnips'

" Snippets are separated from the engine.
Plug 'honza/vim-snippets'

" Trigger configuration. You need to change this to something other than
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsSnippetDirectories=['~/.vim/UltiSnips']

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

call plug#end()

