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
set PATH $PATH $HOME/.rvm/bin 
set PATH $PATH $HOME/.pyenv/bin 
set PATH $PATH /usr/local/opt/coreutils/libexec/gnubin 
set PATH $PATH /usr/local/bin 
set PATH $PATH /usr/local/sbin 
set PATH $PATH /usr/bin 
set PATH $PATH /bin 
set PATH $PATH /usr/sbin 
set PATH $PATH /sbin 
set PATH $PATH /opt/X11/bin 
set PATH $PATH /usr/texbin

set MANPATH $HOME/share/man
set MANPATH $MANPATH $HOME/.cabal/share/man
set MANPATH $MANPATH $HOME/.dotfiles/contrib/visionmedia/git-extras/man
set MANPATH $MANPATH $HOME/.rvm/man
set MANPATH $MANPATH /usr/local/opt/coreutils/libexec/gnuman
set MANPATH $MANPATH /usr/local/share/man
set MANPATH $MANPATH /usr/local/man
set MANPATH $MANPATH /usr/share/man
set MANPATH $MANPATH /opt/X11/share/man
set MANPATH $MANPATH /usr/texbin/man

set PERL5LIB $HOME/.$UNAME.$ARCH/lib64/perl5
set PERL5LIB $PERL5LIB $HOME/.$UNAME.$ARCH/share/perl5

# Activate the default python virtual environment
if test -f $HOME/.venv/$UNAME.$ARCH/bin/activate.fish
    # This is being evaluated BEFORE fish_prompt is declared, so activate.fish
    # messes up my prompt. Disable virtualenv prompt manipulation.
    set VIRTUAL_ENV_DISABLE_PROMPT 1
    source $HOME/.venv/$UNAME.$ARCH/bin/activate.fish
end

set GREP_OPTIONS '--color=auto --exclude-dir=.git --exclude-dir=.svn --exclude-dir=CVS --binary-files=without-match'
set GREP_COLOR '1;32'

# ACK CONFIG
set ACK_COLOR_MATCH 'red'
set ACKRC ~/.dotfiles/ackrc

# LESS CONFIG
set LESS "-RMQ"

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
