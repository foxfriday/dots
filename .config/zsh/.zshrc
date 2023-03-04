# -*- mode: sh; -*-

### Path
typeset -U PATH path  # Discard multiple entries
path+=("${HOME}/.local/bin")
### Helpers
function safe_source { [[ -r "$1" ]] && source "${1}"; }
### Vim
export VIMINIT="set nocp | source ${XDG_CONFIG_HOME:-$HOME/.config}/vim/vimrc"
export GVIMINIT="set nocp | source ${XDG_CONFIG_HOME:-$HOME/.config}/vim/gvimrc"
### Defaults
export COLORTERM=truecolor # Emacs 24-bit color TTY
export EDITOR=vim
export VISUAL=vim
export BROWSER="firefox"   # on WSL "explorer.exe"
export READER="zathura"
export LC_ALL=en_US.UTF-8  # Language and locale
export CLICOLOR=1          # Color
setopt numericglobsort     # Sort filenames numerically when it makes sense
setopt extendedglob        # Globbing allows using regular expressions with *
setopt nocaseglob          # Case insensitive globbing
setopt rcexpandparam       # Array expension with parameters
setopt nobeep
setopt HIST_IGNORE_SPACE
### History
HISTFILE=$ZDOTDIR/.zsh_history
HISTSIZE=1000
setopt appendhistory       # Immediately append history instead of overwriting
setopt histignorealldups   # If a command is a duplicate, remove the older one
### Edit commands in editor C-v
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line
### Prompt
NL=$'\n'
# Re evaluate prompt function
setopt PROMPT_SUBST
function print_prompt {
    if [ -v VIRTUAL_ENV ]; then
        VENV=$(basename "$VIRTUAL_ENV")
        echo "%F{magenta}(${VENV})%f%F{green}%~%f${NL}%(?.%F{blue}>%f.%F{red}>%f)"
    else
        echo "%F{green}%~%f${NL}%(?.%F{blue}>%f.%F{red}>%f)"
    fi
}
PS1=$(print_prompt)
### Directory colors
if [[ -x /usr/bin/dircolors ]]; then
    if [[ -r "${HOME}/.config/zsh/.dircolors" ]]; then
        eval "$(dircolors -b "${ZDOTDIR}"/.dircolors)"
    else
        eval "$(dircolors -b)"
    fi
fi
### Dots
if [[ -d "${HOME}/.dots/bin" ]]; then
    path+=("${HOME}/.dots/bin")
fi
### GNU Pass
if [ -d "$HOME/Documents/pass" ]; then
    export PASSWORD_STORE_DIR="$HOME/Documents/pass"
fi
### Bat
if command -v bat >/dev/null; then
    export MANPAGER="sh -c 'col -bx | bat -l man -p'"
    function battail { tail -f "$1" | bat --paging=never -l log; }
fi
### Pyenv & pipx
if [[ -d "${HOME}/.pyenv/bin" ]]; then
    export PYENV_ROOT="$HOME/.pyenv"
    path+=("${PYENV_ROOT}"/bin)
    # disable prompt mangling when activating an environment
    export VIRTUAL_ENV_DISABLE_PROMPT=1
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
    export PIPX_DEFAULT_PYTHON=$HOME/.pyenv/versions/3.10.9/bin/python3.10
fi
### FZF
# C-r searches history, C-t files, fd to show hidden files
safe_source "${HOME}"/.config/fzf/fzf.zsh
if command -v fd >/dev/null; then
    export FZF_DEFAULT_COMMAND="fd --type f --hidden --no-ignore --follow --exclude .git"
fi
### Aliases
alias cp='cp -i'                      # Confirm before overwriting something
alias mv='mv -i'                      # Confirm bevore overwriting something
alias now='date +%Y-%m-%dT%H:%M%z'
alias grep='grep --color=auto'
alias ls="ls --color=auto"
alias ll="ls -lh"
alias lla="ls -lha --group-directories-first"
alias llt="ls -laht"                  # Time
alias lls="ls -lahS"                  # Size
alias dus="du -ckhs * | sort -hr"     # Size and sort
alias printpath='echo $PATH | tr ":" "\n"'
alias ,show='img2sixel'
alias ,ffile="fzf --preview 'bat --style=numbers --color=always --line-range :500 {}'"
alias ,ffiles="fd --hidden --no-ignore --follow --exclude .git --full-path"
alias ,exifoff="exiftool -all= -tagsfromfile @ -Orientation"
### Autocompletion
# Case insensitive tab completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
# automatically find new executables in path
zstyle ':completion:*' rehash true
# Speed up completions
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.local/zsh/cache
# Don't consider certain characters part of the word
WORDCHARS=${WORDCHARS//\/[&.;]}
# Autocomplete aliases
setopt COMPLETE_ALIASES
# Enable autocompletion (bash should be second)
autoload -Uz compinit
compinit
autoload -Uz bashcompinit
bashcompinit
### Syntax Highlight
export ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR=/usr/share/zsh-syntax-highlighting/highlighters
safe_source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
### GCC
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
### Darwin
[[ $(uname -s) == "Darwin" ]] && safe_source "${ZDOTDIR}"/.darwin
