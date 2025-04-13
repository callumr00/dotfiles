" Install plugin manager.
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Install plugins.
call plug#begin('~/.vim/plugged')
Plug 'sheerun/vim-polyglot'
Plug 'ntk148v/komau.vim'
call plug#end()

colorscheme komau
set termguicolors

set tabstop=4 expandtab shiftwidth=4 shiftround

let &t_SI = "\e[6 q" " Beam-style cursor when inserting.
let &t_SR = "\e[4 q" " Underline-style cursor when replacing.
let &t_EI = "\e[2 q" " Block-style cursor elsewhere.

let g:netrw_banner=0 " Remove file browser banner.
