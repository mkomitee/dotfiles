syn match myDate "<\d\{4}-\d\{2}-\d\{2} ...>"

syn match myTODO "\<TODO\>" containedin=markdownH. contained
syn match myWAITING "\<WAITING\>" containedin=markdownH. contained
syn match myHOLD "\<HOLD\>" containedin=markdownH. contained
syn match myDONE "\<DONE\>" containedin=markdownH. contained
syn match myCANCELED "\<CANCELED\>" containedin=markdownH. contained
syn match myPriority "\[\#\d\+\]" containedin=markdownH. contained

syn match myTODO "\<TODO\>" containedin=htmlH. contained
syn match myWAITING "\<WAITING\>" containedin=htmlH. contained
syn match myHOLD "\<HOLD\>" containedin=htmlH. contained
syn match myDONE "\<DONE\>" containedin=htmlH. contained
syn match myCANCELED "\<CANCELED\>" containedin=htmlH. contained
syn match myPriority "\[\#\d\+\]" containedin=htmlH. contained

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

hi def link myTODO GruvboxRedBold
hi def link myWAITING GruvboxOrangeBold
hi def link myHOLD GruvboxYellowBold
hi def link myDONE GruvboxGreenBold
hi def link myCANCELED Comment
hi def link myDate Comment
hi def link myPriority Comment
