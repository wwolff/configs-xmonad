" ~/.vimrc

" ---[ TODO ]------------------------------------------------
"  clean up augroups
" -----------------------------------------------------------

set spellfile=/home/gvg/.vim/spell/en.utf-8.add

" Look at whole file syntax rather than `seen' portion nearby
:au Syntax * syntax sync fromstart
syntax on

" ---[ `Pathogen' easy install/uninstall from vim/bundles ]--
"         Using http://github.com/tpope/vim-pathogen
"         Any plugin, uncompressed, {svn,git}-checkout, git-clone in
"         vim/bundle will get added to runtime path...
"         NOTE: ensure pathogen runs before filetype plugin!
call pathogen#runtime_append_all_bundles()
filetype plugin on
filetype indent on

set t_Co=256
set bg=dark
colorscheme desert
"colorscheme zellner
if &diff
    colorscheme zenburn
endif

"let g:xml_syntax_folding = 1

" ---[ Set Set Set ]--- {{{
set bs=2 ls=2 shm=at tw=72 ww=<,>,h,l
"set bs=2 fo=cqrt ls=2 shm=at tw=72 ww=<,>,h,l
set cmdheight=2
set completeopt=menu,menuone
set cursorline
set digraph ek hidden ruler sc vb wmnu
set expandtab
"set formatoptions=tacq
set formatoptions=tcq
set grepprg=grep\ -nH\ $*
set incsearch
set lazyredraw
set modeline modelines=2
set nocp
set noeb noet nosol
set number
set shell=/bin/bash\ -i
set shiftwidth=4
set showcmd
set showmatch
set showmode
set statusline=%<%f\ %y[%{&ff}]%m%r%w%a\ %=%l/%L,%c%V\ %P
set tabstop=4
"set tabstop=2 softtabstop=2 shiftwidth=2 expandtab
"set tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab
set viminfo=%,'50,\"100,:100,n~/.viminfo
set visualbell
set wildchar=<Tab>
set wildmenu
set wildmode=longest:full,full
" }}}

" ---[ Key Maps ]--- {{{

nmap <silent> <Up> :wincmd k<CR>
nmap <silent> <Down> :wincmd j<CR>
nmap <silent> <Left> :wincmd h<CR>
nmap <silent> <Right> :wincmd l<CR>

" <F1> <F2> <F3> unused

map  <F4> :set paste! <Bar> set paste?<CR>
imap <F4> <ESC>:set paste!<CR>i

map  <F5> :set number! <Bar> set number?<CR>
imap <F5> <ESC>:set number!<CR>i

" insert date/time or date
:map <F6> a<C-R>=strftime("%F %T")<CR><Esc>
:map <F7> a<C-R>=strftime("%F")<CR><Esc>

" clear highlighting
map <silent> <F8> :nohlsearch<CR>

" sort words on current line
" ( if quoted bits further right in the line, vi",s or vi',s will also work)
nmap <F9> V :SortWords<CR>
" or selected words
vmap <F9> :SortWords<CR>

noremap <silent> <F10> :call VimCommanderToggle()<CR>
"     VimCommander keys {{{
"   - TAB       = Go to the other panel.
"   - F3        = View file under cursor.
"   - F4        = Edit file under cursor.
"   - S-F4      = Edit new file.
"   - F5        = Copy file.
"   - F6        = Move/rename file.
"   - F7        = Create directory.
"   - F8/DEL    = Remove file.
"   - F10       = Quit VimCommander.
"   - C-R       = Refresh panels.
"       - Backspace = Go to parent directory.
"   - C-U       = Exchange panels.
"   - C-Left    = Put directory under cursor on other panel, or grab
"                 other panel's dir.
"   - C-Right   = Same.
"   - \H        = Show hidden files (toggle).
"   - INS       = Select file under cursor.
"   - "+"       = Select file by pattern.
"   - "-"       = De-select file by pattern.
"   - C-t       = Previous directory.
"   - C-y       = Next directory.
"   }}}

" <F11> is unused

" Strip trailing whitespace
map  <silent> <F12> :% s/\s\+$//<cr>

" }}}

" ---[ Functions ]--- {{{

" http://stackoverflow.com/questions/1327978/sorting-words-not-lines-in-vim
command! -nargs=0 -range SortWords call SortWords()
function! SortWords()
    " Get the visual mark points
    let StartPosition = getpos("'<")
    let EndPosition = getpos("'>")

    if StartPosition[0] != EndPosition[0]
        echoerr "Range spans multiple buffers"
    elseif StartPosition[1] != EndPosition[1]
        " This is a multiple line range, probably easiest to work line wise

        " This could be made a lot more complicated and sort the whole
        " lot, but that would require thoughts on how many
        " words/characters on each line, so that can be an exercise for
        " the reader!
        for LineNum in range(StartPosition[1], EndPosition[1])
            call setline(LineNum, join(sort(split(getline('.'), ' ')), " "))
        endfor
    else
        " Single line range, sort words
        let CurrentLine = getline(StartPosition[1])

        " Split the line into the prefix, the selected bit and the suffix

        " The start bit
        if StartPosition[2] > 1
            let StartOfLine = CurrentLine[:StartPosition[2]-2]
        else
            let StartOfLine = ""
        endif
        " The end bit
        if EndPosition[2] < len(CurrentLine)
            let EndOfLine = CurrentLine[EndPosition[2]:]
        else
            let EndOfLine = ""
        endif
        " The middle bit
        let BitToSort = CurrentLine[StartPosition[2]-1:EndPosition[2]-1]

        " Move spaces at the start of the section to variable StartOfLine
        while BitToSort[0] == ' '
            let BitToSort = BitToSort[1:]
            let StartOfLine .= ' '
        endwhile
        " Move spaces at the end of the section to variable EndOfLine
        while BitToSort[len(BitToSort)-1] == ' '
            let BitToSort = BitToSort[:len(BitToSort)-2]
            let EndOfLine = ' ' . EndOfLine
        endwhile

        " Sort the middle bit
        let Sorted = join(sort(split(BitToSort, ' ')), ' ')
        " Reform the line
        let NewLine = StartOfLine . Sorted . EndOfLine
        " Write it out
        call setline(StartPosition[1], NewLine)
    endif
endfunction
" }}}

" ---[ Autocommands ]--- {{{

autocmd ColorScheme * highlight TrailingWhitespace ctermbg=red guibg=red

" Haskell and haskellmode prefs
augroup Haskell-Files
au BufEnter *.hs compiler ghc
au BufEnter *.lhs compiler ghc
au Syntax * syn match TrailingWhitespace /.\+\s\+$/
au Syntax * syn match BadWhitespace /\t\+/
au Syntax * syn match BadWhitespace /.*\t\+/
set path=.,~/repos/xmonad,~/repos/XMonadContrib,~/repos/xmobar,,
set tags=~/repos/**/tags

let g:haddock_browser="firefox"
"let g:ghc="/usr/bin/ghc"
"let g:haddock_docdir="/usr/share/doc/ghc-6.10.4/"
"let g:haddock_indexfiledir="/home/gvg/share/doc/"

"To highlight bad whitespace
"autocmd ColorScheme * highlight BadWhitespace ctermfg=red ctermbg=yellow guibg=yellow

set textwidth=80
set expandtab
augroup END

" sup email prefs
augroup sup email
au BufRead sup.* set ft=mail

"Highlight bad whitespace
"autocmd ColorScheme * highlight BadWhitespace ctermfg=red ctermbg=yellow guibg=yellow

au Syntax * syn match TrailingWhitespace /.\+\s\+$/
"au Syntax * syn match BadWhitespace /\t\+/
"au Syntax * syn match BadWhitespace /.*\t\+/
" TODO
set textwidth=68
set expandtab
augroup END

" xmobarrc prefs
augroup rc-files
au BufRead *mobarrc* set ft=sh
"Highlight trailing whitespace and tab chars
"au Syntax * syn match BadWhitespace /\t\+/
"au Syntax * syn match BadWhitespace /.*\t\+/
au Syntax * syn match TrailingWhitespace /.\+\s\+$/
" TODO
set expandtab
augroup END

" C and C++ prefs.
augroup C-Files
  au!
  au BufNewFile,BufRead *.c,*.C,*.cc set cindent number
  au BufNewFile *.c 0r ~/.vim/skeleton/skel.c
  "au FileType cpp,c,h set foldenable foldmethod=syntax
  au FileType cpp,c,h set comments=sl:/*,mb:**,elx:*
  au FileType cpp,c,h set foldcolumn=2
  au FileType cpp,c,h set shiftwidth=4
  "au FileType cpp,c,h syn region Block start="{" end="}" transparent fold
  " Abbreviations
  ab #d #define
  ab #i #include<
augroup END

" Latex prefs.
augroup Tex-Files
  au!
  au BufNewFile,BufRead *.tex set tabstop=4 smartindent
  au BufNewFile *.tex 0r ~/.vim/skeleton/skel.tex
  au FileType tex ia fr \dst\frac
  au Filetype tex ia fb \fboxsep=.2in \framebox{
augroup END

" Mail and News
au FileType mail set tw=72 formatoptions=tcq comments=n:>,n::,n:#,n:% digraph

" Perl
augroup Perl
    au!
    au BufNewFile,BufRead *.pl,*.pm set smartindent
augroup END
" }}}

" vim:foldmethod=marker
