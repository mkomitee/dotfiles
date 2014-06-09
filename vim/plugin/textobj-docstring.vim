function! PyDocstrTxtObj (inner)
    " TEXT OBJECT FOR IN/AROUND PYTHON DOCSTRING
    "
    " For docstrings in this format:
    " ,------------------------------.
    " | """                          |
    " | Module-level docstring.      |
    " | Text object works on these.  |
    " | """                          |
    " |                              |
    " | class Widget (object):       |
    " | """                          |
    " | Text object also works       |
    " | on class-level docstrings.   |
    " | """                          |
    " |     def __init__ (self):     |
    " |         """                  |
    " |         Method-level, too!   |
    " |         """                  |
    " |         pass                 |
    " |                              |
    " '------------------------------'
    " Text objects search up, but won't cross def/class lines.
    "
    " get current line number
    let s = line('.')
    " climb up to first def/class line, or first line of buffer
    while s > 0 && getline(s) !~ '^\s*\(def\|class\)'
        let s = s - 1
    endwhile
    " set search start to just after def/class line, or on first buffer line
    let s = s + 1
    " descend lines until end of buffer or def/class line
    while s < line('$') && getline(s) !~ '^\s*\(def\|class\)'
        " if line begins with optional whitespace followed by """
        if getline(s) =~ "^\\s*\"\"\""
            " set search end to just after found start line
            let e = s + 1
            " descend lines until end of buffer or def/class line
            while e <= line('$') && getline(e) !~ '^\s*\(def\|class\)'
                " if line ends with """ followed by optional whitespace
                if getline(e) =~ "\"\"\"\\s*$"
                    " TODO check first for blank lines above to select instead
                    " for 'around', extend search end through blank lines
                    if !a:inner
                        let x = e + 1
                        while x <= line('$') && getline(x) =~ '^\s*$'
                            let e = x
                            let x = x + 1
                        endwhile
                    endif
                    " visual line select from start to end (first cursor move)
                    exe 'norm '.s.'ggV'.e.'gg'
                    return
                endif
                " move search end down a line
                let e = e + 1
            endwhile
        endif
        " move search start down a line
        let s = s + 1
    endwhile
endfunction

" map in/around python docstring text objects
onoremap <silent>ad :<C-u>cal PyDocstrTxtObj(0)<CR>
onoremap <silent>id :<C-u>cal PyDocstrTxtObj(1)<CR>
vnoremap <silent>ad :<C-u>cal PyDocstrTxtObj(0)<CR>
vnoremap <silent>id :<C-u>cal PyDocstrTxtObj(1)<CR>
