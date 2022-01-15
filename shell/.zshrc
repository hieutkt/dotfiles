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
