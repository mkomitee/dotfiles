if &background == 'light'
    hi SignColumn    ctermfg=10 ctermbg=15
    hi ShowMarksHLl  ctermfg=1  ctermbg=15
    hi ShowMarksHLu  ctermfg=9  ctermbg=15
    hi ShowMarksHLo  ctermfg=5  ctermbg=15
    hi ShowMarksHLm  ctermfg=6  ctermbg=15
    hi StatusLine    ctermbg=3  ctermfg=7
    hi StatusLineNC  ctermbg=2  ctermfg=7
else
    hi SignColumn    ctermfg=10 ctermbg=0
    hi ShowMarksHLl  ctermfg=1  ctermbg=0
    hi ShowMarksHLu  ctermfg=9  ctermbg=0
    hi ShowMarksHLo  ctermfg=5  ctermbg=0
    hi ShowMarksHLm  ctermfg=6  ctermbg=0
    hi StatusLine    ctermbg=3  ctermfg=0
    hi StatusLineNC  ctermbg=3  ctermfg=8
end
