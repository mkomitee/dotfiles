which fasd > /dev/null 2>&1
if [ $? -eq 0 ]; then
    eval "$(fasd --init auto)"
    alias v='f -e vim'
else
    alias v='echo fasd unavailable'
fi
