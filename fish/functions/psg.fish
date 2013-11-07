function psg
    ps -eo pid,user,args | grep --color=always $argv | grep -v grep | collapse
end
