syn match myDate "<\d\{4}-\d\{2}-\d\{2} ...>"

syn match myTODO "\<TODO\>" containedin=pandocAtxHeader contained
syn match myWAITING "\<WAITING\>" containedin=pandocAtxHeader contained
syn match myHOLD "\<HOLD\>" containedin=pandocAtxHeader contained
syn match myDONE "\<DONE\>" containedin=pandocAtxHeader contained
syn match myCANCELED "\<CANCELED\>" containedin=pandocAtxHeader contained
syn match myPriority "\[\#\d\+\]" containedin=pandocAtxHeader contained

syn match myTODO "\<TODO\>" containedin=pandocNoFormatted contained
syn match myWAITING "\<WAITING\>" containedin=pandocNoFormatted contained
syn match myHOLD "\<HOLD\>" containedin=pandocNoFormatted contained
syn match myDONE "\<DONE\>" containedin=pandocNoFormatted contained
syn match myCANCELED "\<CANCELED\>" containedin=pandocNoFormatted contained
syn match myPriority "\[\#\d\+\]" containedin=pandocNoFormatted contained

" syn match myDESFLOW "<#desflow .*>"
syn match myDESFLOW "<#desflow .\{-}>"

" hi def link myDESFLOW pandocReferenceURL
hi def link myDESFLOW pandocLinkTip

hi def link myCANCELED Comment
hi def link myDate Comment
hi def link myPriority Comment
hi def link myTODO Todo
hi def link myWAITING Todo
hi def link myHOLD Todo
hi def link myDONE Todo

if g:colors_name == "gruvbox"
  hi def link myTODO GruvboxRedBold
  hi def link myWAITING GruvboxOrangeBold
  hi def link myHOLD GruvboxYellowBold
  hi def link myDONE GruvboxGreenBold
endif
