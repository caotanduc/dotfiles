"   ____    _    ___    _____  _    _   _   ____  _   _  ____ 
"  / ___|  / \  / _ \  |_   _|/ \  | \ | | |  _ \| | | |/ ___|
" | |     / _ \| | | |   | | / _ \ |  \| | | | | | | | | |    
" | |___ / ___ \ |_| |   | |/ ___ \| |\  | | |_| | |_| | |___ 
"  \____/_/   \_\___/    |_/_/   \_\_| \_| |____/ \___/ \____|
                                                            
set rtp+=/opt/homebrew/opt/fzf

" gui
" set guifont=SourceCodePro-Regular:h16
set guifont=Iosevka-Light-Extended:h16

" color
set t_Co=256

set omnifunc=syntaxcomplete#Complete
syntax off

" if return 0 or appear '-clipboard' set clipboard is unmeaned
" check with echo has('clipboard') or vim --version | grep clipboard 
set clipboard=unnamed

" use vim as default not vi (vim-tiny)
set nocompatible
filetype plugin indent on

" no creating swapfile (recorver) when open a new buffer
set noswapfile 
autocmd SwapExists * let v:swapchoice='o'

" no notify when open/edit a new file when current file is not save
set hidden

" work with find cmd, find in current working directory and recursive inside that current directory
set path=.,**

" these settings exactly like default
set showcmd
set ruler
set notimeout
set nobackup

" file saving format
set encoding=UTF-8
scriptencoding UTF-8
set fileencoding=UTF-8

" using mouse in vim
set mouse=a

" wild menu: showing completion options for cmd and file name in cmd line
set wildmenu

" the cursor never touch the last line in the screen
set scrolloff=8

" turn off sound
set noerrorbells
set vb t_vb= 


set laststatus=1
set noshowmode

" netrw 
" toggle banner hit "I"
let g:netrw_keepdir=0
let g:netrw_banner=0
let g:netrw_liststyle=3
nnoremap <leader>nt <cmd>vsplit<cmd>e .<cr>
let g:netrw_browse_split=4
" autocmd VimEnter * if expand("%") == "" | e. | endif

" FOR EDITING EXPERIENCE

" show line number
set number

" smart tab (ex. inside for loop, insert 1 tab)
set smarttab

" no wrap line
set nowrap

" hitting (>>) or (<<) to indent a line shift with 4 space(a tab)
set shiftwidth=4 tabstop=4 softtabstop=4 smartindent
set mps+=<:>

" appear signcolumn when using lsp
set signcolumn=auto
" make signcolumn same color with line number background
highlight clear SignColumn

" filetype separate config
autocmd Filetype markdown : set wrap

" realtime file update
set autoread | autocmd CursorHold * checktime | call feedkeys("lh")

" auto change vim working directory
set autochdir | autocmd bufenter * silent! lcd %:p:h


" working as startup
if has('vim_starting')
	set cursorline
	set relativenumber
endif

autocmd InsertEnter * silent! set norelativenumber nocursorline
autocmd InsertLeave * silent! set relativenumber cursorline

" highlight words in column greater than 80
highlight ColorColumn ctermbg=232
call matchadd('ColorColumn', '\%81v', 100)

set incsearch nohlsearch

" highlight LineNr cterm=none ctermbg=black ctermfg=242
" highlight CursorLineNr cterm=bold ctermfg=220 ctermbg=black
highlight LineNr cterm=none ctermbg=black ctermfg=242
highlight CursorLineNr cterm=bold ctermfg=64 ctermbg=black

highlight Visual cterm=none ctermbg=242

" mappings
let mapleader=" "
inoremap jk <esc>
nnoremap <C-h> :set hlsearch!<cr>
nnoremap <leader>nt <cmd> vsplit . <cr> <cmd> vertical resize 30 <cr>
nnoremap <leader>so <cmd>source	~/.vimrc<cr>


nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <C-S-tab> :tabprevious<CR>
nnoremap <C-tab>   :tabnext<CR>
nnoremap <C-t>     :tabnew<CR>
inoremap <C-S-tab> <Esc>:tabprevious<CR>i
inoremap <C-tab>   <Esc>:tabnext<CR>i
inoremap <C-t>     <Esc>:tabnew<CR>

" Normal mode mappings
nnoremap <S-down> :m .+1<CR>==
nnoremap <S-up> :m .-2<CR>==

" Insert mode mappings
inoremap <S-down> <Esc>:m .+1<CR>==gi
inoremap <S-up> <Esc>:m .-2<CR>==gi

" Visual mode mappings
vnoremap <S-down> :m '>+1<CR>gv=gv
vnoremap <S-up> :m '<-2<CR>gv=gv

nnoremap <C-a> gg<S-v>G

" call plug#begin()

" Plug 'tpope/vim-surround'
" Plug 'tpope/vim-repeat'
" Plug 'itchyny/lightline.vim'
" Plug 'ervandew/supertab'
" Plug 'morhetz/gruvbox'
" Plug 'davidhalter/jedi-vim'
" 
" call plug#end()

if has("gui_running")
	set termguicolors
	set background=light
	colorscheme gruvbox
	let g:gruvbox_contrast_light='hard'
endif

