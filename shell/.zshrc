# Variables
export ZSH="$HOME/.oh-my-zsh"
export ZSH_THEME="robbyrussell"
export PATH="/usr/local/stata15:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export POP_PLUGINS_PATH="$HOME/.local/share/pop-launcher/plugins"
export FZF_DEFAULT_OPTS='--layout=reverse --border'
export LANG=en_US.UTF-8
export EDITOR='emacsclient'

# Aliases
alias cat="batcat"

# Critical external app setup
# Oh-my-Zsh
plugins=(git)
source $ZSH/oh-my-zsh.sh
# fzf integration
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Document find in Zotero
# Integration between ripgrep-all and fzf
docfind () {
    RG_PREFIX="rga --files-with-matches  --type pdf"
    local file
    file="$(cd ~/Dropbox/Documents/Zotero/storage/ &&
        FZF_DEFAULT_COMMAND="$RG_PREFIX '$1'" \
            fzf --sort --preview="[[ ! -z {} ]] && rga --pretty --context 5 {q} {}" \
            --phony -q "$1" \
            --bind "change:reload:$RG_PREFIX {q}" \
            "--preview-window="70%)" &&
    printf "Opening \033[0;34m$file" &&
    nohup xdg-open "$HOME/Dropbox/Documents/Zotero/storage/$file"
}
