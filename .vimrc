
" Source that made possible the current configuration:
"
" * https://stackoverflow.com/q/164847
"

" Disable compatibility with vi which can cause unexpected issues.
set nocompatible

" `cd` into the directory containing the file you are working on.
set autochdir

" Enable type file detection.
filetype on

" Load plugins for the detected file type.
filetype plugin on

" Turn syntax highlighting on.
syntax on

" Disable bips. It is annoying.
set noerrorbells

" Add numbers to each line on the left-hand side.
set number

" Highlight cursor line underneath the cursor horizontally.
set cursorline

" Highlight cursor line underneath the cursor vertically.
"set cursorcolumn

" Indentation.
filetype plugin indent on
set autoindent
set expandtab
set smarttab
set shiftwidth=2
set softtabstop=2


" How many columns per line.
"set textwidth=75

" Draw a vertical rule to curb the lenght of the lines. However, lines will
" not be broken into two lines if such limit is exceeded.
set colorcolumn=76

" Do not save backup files.
set nobackup

" Do not let cursor scroll below or above N number of lines when scrolling.
set scrolloff=10

" Do not wrap lines. Allow long lines to extend as far as the line goes.
"set nowrap

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

" Plugins for vim.
call plug#begin('~/.vim/plugged')

" TeX and LaTeX
Plug 'lervag/vimtex'
let g:livepreview_previewer = 'zathura'
let g:vimtex_view_general_options = '--unique file:@pdf\#src:@line@tex'
let g:tex_flavor='latex'

" A Vim Plugin for Lively Previewing LaTeX PDF Output
Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }
let g:livepreview_previewer = 'zathura'
let g:livepreview_engine = 'lualatex'
let g:livepreview_use_biber = 1
let g:livepreview_cursorhold_recompile = 0

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

" Markdown
Plug 'preservim/vim-markdown'
let g:vim_markdown_folding_disabled = 1

" Vim's colorscheme: some other cool color shemes are 'sainnhe/everforest'
" and 'doums/darcula'. Other colorschmes here: https://vimcolorschemes.com/
Plug 'sainnhe/gruvbox-material'

" Some airline for Vim
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
let g:airline_theme='base16_gruvbox_dark_hard'

call plug#end()

set background=dark
colorscheme gruvbox-material

