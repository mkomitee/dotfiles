function fish_user_key_bindings --description 'User key bindings for fish'
    if functions -q fish_vi_key_bindings
        set fish_key_bindings fish_vi_key_bindings
        bind -M insert \ej history-token-search-forward
        bind -M insert \ek history-token-search-backward
        bind -M default \ej history-token-search-forward
        bind -M default \ek history-token-search-backward
        bind \cc -M visual -m default kill-whole-line force-repaint
        bind \cc -M insert kill-whole-line force-repaint
        bind \cc -M default kill-whole-line force-repaint
    end
    set -g installed_user_key_bindings 1
end
