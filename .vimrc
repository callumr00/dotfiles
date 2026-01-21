" Plugin manager
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Plugins
call plug#begin('~/.vim/plugged')
Plug 'sheerun/vim-polyglot'
Plug 'ntk148v/komau.vim'
call plug#end()

" Colour scheme
colorscheme komau
set termguicolors

" Indentation
set tabstop=4
set expandtab
set shiftwidth=4
set shiftround

" Search
set hlsearch
set incsearch

" Disable arrow keys in insert mode
inoremap <Up>    <Nop>
inoremap <Down>  <Nop>
inoremap <Left>  <Nop>
inoremap <Right> <Nop>

" Change cursor by mode
let &t_SI = "\e[6 q" " Beam-style cursor when inserting
let &t_SR = "\e[4 q" " Underline-style cursor when replacing
let &t_EI = "\e[2 q" " Block-style cursor elsewhere

" Other
let g:netrw_banner=0 " Remove file browser banner
