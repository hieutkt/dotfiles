# Setup fzf
# ---------
if [[ ! "$PATH" == */home/hieuphay/.fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}/home/hieuphay/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/hieuphay/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/hieuphay/.fzf/shell/key-bindings.zsh"
