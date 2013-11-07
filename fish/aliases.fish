function vi; vim $argv; end
function bc; command bc -l $argv; end
function a; resume main; end
function tig; command tig --all $argv; end
function tmux; command tmux -u2 $argv; end
function vim-clear-cache; find ~/.vimdata -type f -delete; end
function kcurl; curl --negotiate --user : $argv; end
function irc; ssh -t matrix.gs tmux -u2 attach-session -t irc; end
function reexec; exec fish -l; end
function ef; vim ~/.config/fish/config.fish; end
function ev; vim ~/.vimrc; end
function ed; vim ~/.vim/en.utf-8.add; end
function ez; vim ~/.zshrc; end
function et; vim ~/.tmux.conf; end
function eg; vim ~/.gitconfig; end
function cuts; cut -d' ' $argv; end
