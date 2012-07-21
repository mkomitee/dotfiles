#!/bin/zsh

function tex2() {
    if [ -f "$2.tex" ]; then
        case "$1" in
            pdf)
                if [ -f $2.pdf ]; then
                    echo "$2.pdf already exists."
                else
                    pdflatex --halt-on-error $2.tex > /dev/null
                fi
                ;;
            dzslides|html|html+lhs|html5|html5+lhs|s5|slidy)
                if [ -f $2.html ]; then
                    echo "$2.html already exists."
                else
                    pandoc -f latex -s $2.tex -t $1 -o $2.html
                fi
                ;;
            *)
                if [ -f $2.$1 ]; then
                    echo "$2.$1 already exists."
                else
                    pandoc -f latex -s $2.tex -t $1 -o $2.$1
                fi
                ;;
        esac
        if [ "$?" != "0" ]; then
            echo "Error generating file."
        fi
    else
        echo "$2.tex doesn't exist."
    fi
}
