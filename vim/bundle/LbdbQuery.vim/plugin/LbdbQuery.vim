" FILE: LbdbQuery.vim
" AUTHOR: Janakiraman .S <prince AT india DoT ti dOt com>
" Last Modified: Wed, 29 Oct 2003 08:48:16 (IST)
" Usage: Just source this file.
"        source LbdbQuery.vim
" LICENSE: Same as Vim. Use at  your own risk. Add stuff to taste.
"          If you like this script type :help uganda<Enter>
" NOTES: This script invokes lbdbp to look up the email address. mutt-heads
"        that edit headers with vim might find this one useful. The results
"        are displayed in a split window. The window is closed when the key
"        'q' is hit. Hit the "+" key to select the email id of interest.
"        Written to work with vim-5.7.
" FIXES: Script doesn't work if name is of form "Lastname, Firstname"
"        Fix contributed by Gerhard Siegesmund <gerhard DoT siegesmund AT orange-digital dOT de>

" Map the key combination ,i to look up the current word with lbdbq.
map ,i :call LbdbQuery(expand("<cword>"))<cr>

func! LbdbQuery ( username )
  echo a:username
  if ( bufexists ("_lbdbquerybuf.tmp") )
    let a = bufwinnr("_lbdbquerybuf.tmp")
    if ( a == -1 )
      sb _lbdbquerybuf.tmp
    else
      execute "normal ".a."\<C-w>w"
    endif
  else
    sp _lbdbquerybuf.tmp
  endif
  normal ggdG
  let old_Report = &report
  let &report = 1000
  exec "r!lbdbq "a:username
  let &modified=0
endfunction

fun! LbdbSelectEntry()
  normal _
  exec "normal \"adt\<Tab>"
  let @a = substitute(@a,'^',"<","")
  let @a = substitute(@a,'$',">","")
  let mailid = @a
  normal x
  exec "normal \"adt\<Tab>"
  let username = @a
  let @a = "\"".username ."\" ". mailid
  undo
  let &modified=0
  hide
  normal diw
  if (col (".") == 1)
    exec "normal i\<C-R>a\<Esc>"
  else
    exec "normal a\<C-R>a\<Esc>"
  endif
endfunction

augroup LbdbQueryStuff
  au!
  au BufEnter _lbdbquerybuf.tmp let &swapfile=0
  au BufEnter _lbdbquerybuf.tmp nm q :hide<CR>
  au BufLeave _lbdbquerybuf.tmp nun q
  au BufEnter _lbdbquerybuf.tmp nm + :call LbdbSelectEntry()<cr>
  au BufLeave _lbdbquerybuf.tmp nun +
augroup end
