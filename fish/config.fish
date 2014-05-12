set fish_greeting ""

set UNAME (uname)
set ARCH (arch)
set GOROOT $HOME/.$UNAME.$ARCH/go

set PATH $HOME/bin 
set PATH $PATH $HOME/.cabal/bin 
set PATH $PATH $HOME/scripts 
set PATH $PATH $HOME/.$UNAME.$ARCH/bin 
set PATH $PATH $GOROOT/bin 
set PATH $PATH $HOME/.dotfiles/contrib/visionmedia/git-extras/bin 
set PATH $PATH $HOME/.dotfiles/contrib/willgit/mainline/bin 
set PATH $PATH $HOME/.gem/ruby/1.8/bin
set PATH $PATH $HOME/.rvm/bin 
set PATH $PATH $HOME/.pyenv/bin 
set PATH $PATH /usr/local/java/jdk/bin
set PATH $PATH /usr/local/opt/coreutils/libexec/gnubin 
set PATH $PATH /usr/local/bin 
set PATH $PATH /usr/local/sbin 
set PATH $PATH /usr/bin 
set PATH $PATH /bin 
set PATH $PATH /usr/sbin 
set PATH $PATH /sbin 
set PATH $PATH /opt/X11/bin 
set PATH $PATH /usr/texbin
set PATH $PATH /opt/bin
set PATH $PATH /prod/systems/bin
set PATH $PATH /proj/systems/bin
set PATH $PATH /proj/systems/etc
set PATH $PATH /proj/systems/network/bin
set PATH $PATH /prod/wiki/bin

set MANPATH $HOME/share/man
set MANPATH $MANPATH $HOME/.cabal/share/man
set MANPATH $MANPATH $HOME/.dotfiles/contrib/visionmedia/git-extras/man
set MANPATH $MANPATH $HOME/.rvm/man
set MANPATH $MANPATH $HOME/.$UNAME.$ARCH/share/man
set MANPATH $MANPATH /usr/local/java/jdk/man
set MANPATH $MANPATH /usr/local/opt/coreutils/libexec/gnuman
set MANPATH $MANPATH /usr/local/share/man
set MANPATH $MANPATH /usr/local/man
set MANPATH $MANPATH /usr/share/man
set MANPATH $MANPATH /opt/X11/share/man
set MANPATH $MANPATH /usr/texbin/man
set MANPATH $MANPATH /opt/share/man

set PERL5LIB $HOME/.$UNAME.$ARCH/lib64/perl5
set PERL5LIB $PERL5LIB $HOME/.$UNAME.$ARCH/share/perl5

set GREP_OPTIONS '--color=auto --exclude-dir=.git --exclude-dir=.svn --exclude-dir=CVS --binary-files=without-match'
set GREP_COLOR '1;32'

# ACK CONFIG
set ACK_COLOR_MATCH 'red'
set ACKRC ~/.dotfiles/ackrc

# LESS CONFIG
set LESS "-RMQigMws"

set PAGER less
set EDITOR vim
set SVNEDITOR $EDITOR
set VISUAL $EDITOR

set PYTHONDONTWRITEBYTECODE 1
set PEP8_ARGS '--ignore=E501'
set PYLINT_ARGS '-d C0301,W0142'

set LC_ALL en_US.UTF-8
set LANG en_US.UTF-8

source $HOME/.config/fish/aliases.fish

if status --is-interactive
    source $HOME/.config/fish/interactive.fish
end

if test -f $HOME/.config/fish/local.fish
    source $HOME/.config/fish/local.fish
end
