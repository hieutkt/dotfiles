# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Variables
export ZSH="$HOME/.oh-my-zsh"
export ZSH_THEME="powerlevel10k/powerlevel10k"
export PATH="/usr/local/stata15:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="/usr/local/texlive/2023/bin/x86_64-linux:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export POP_PLUGINS_PATH="$HOME/.local/share/pop-launcher/plugins"
export FZF_DEFAULT_COMMAND="fd --type file --follow --hidden --exclude .git --color=always"
export FZF_DEFAULT_OPTS="--ansi --layout=reverse --border --preview 'bat --color=always --style=header,grid --line-range :300 {}'"
export LANG=en_US.UTF-8
export EDITOR='emacsclient'

# Aliases
if which bat    &> /dev/null ; then alias cat="bat"   ; fi
if which batcat &> /dev/null ; then alias cat="batcat"; fi
if which fdfind &> /dev/null ; then alias  fd="fdfind"; fi

# Critical external app setup
# Oh-my-Zsh
plugins=(git zsh-autosuggestions zsh-syntax-highlighting)
source $ZSH/oh-my-zsh.sh
# fzf integration
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export PATH=$PATH:/home/hieuphay/.spicetify

rga-fzf() {
    RG_PREFIX="rga --files-with-matches"
    local file
    file="$(
        FZF_DEFAULT_COMMAND="$RG_PREFIX '$1'" \
            fzf --sort --preview="[[ ! -z {} ]] && rga --pretty --context 5 {q} {}" \
                --phony -q "$1" \
                --bind "change:reload:$RG_PREFIX {q}" \
                --preview-window="70%:wrap"
    )" &&
    echo "opening $file" &&
    xdg-open "$file"
}

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
