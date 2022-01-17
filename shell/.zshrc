# Variables
export ZSH="$HOME/.oh-my-zsh"
export ZSH_THEME="robbyrussell"
export PATH="/usr/local/stata15:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export POP_PLUGINS_PATH="$HOME/.local/share/pop-launcher/plugins"
export FZF_DEFAULT_COMMAND="fdfind --type file --follow --hidden --exclude .git --color=always"
export FZF_DEFAULT_OPTS="--ansi --layout=reverse --border --preview 'batcat --color=always --style=header,grid --line-range :300 {}'"
export LANG=en_US.UTF-8
export EDITOR='emacsclient'

# Aliases
if which batcat &> /dev/null; then alias cat="batcat"; fi
if which fdfind &> /dev/null; then alias  fd="fdfind"; fi

# Critical external app setup
# Oh-my-Zsh
plugins=(git)
source $ZSH/oh-my-zsh.sh
# fzf integration
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
