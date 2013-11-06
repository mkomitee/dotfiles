source $HOME/.config/fish/lib/vi-mode.fish

function fish_right_prompt -d "Write out the right prompt"
        printf $vi_mode
end
