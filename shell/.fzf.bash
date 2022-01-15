# Setup fzf
# ---------
if [[ ! "$PATH" == */home/hieuphay/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/home/hieuphay/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/hieuphay/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/hieuphay/.fzf/shell/key-bindings.bash"
