# ~/.bashrc: executed by bash(1) for non-login shells.

## Check interactive
case $- in
    *i*) ;;
      *) return;;
esac

## Settings
HISTCONTROL=ignoreboth          # no duplicates or starting with space
shopt -s histappend             # append history
HISTSIZE=1000
HISTFILESIZE=2000
shopt -s checkwinsize           # update LINE and COLUMNS
# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

## Color support
case "$TERM" in
    xterm-color|*-256color|foot)
        PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\n>\[\033[00m\]'
        export COLORTERM=truecolor # Emacs 24-bit color TTY
        export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
        alias grep='grep --color=auto'
        alias ls="ls --color=auto"
        ;;
    *)
        PS1='\u@\h:\w\n> '
        ;;
esac
if [ -x /usr/bin/dircolors ]; then
    if [ -r "${HOME}/.config/zsh/.dircolors" ]; then
        eval "$(dircolors -b ~/.config/zsh/.dircolors)"
    else
        eval "$(dircolors -b)"
    fi
fi
### Vim
export VIMINIT="set nocp | source ${XDG_CONFIG_HOME:-$HOME/.config}/vim/vimrc"
export GVIMINIT="set nocp | source ${XDG_CONFIG_HOME:-$HOME/.config}/vim/gvimrc"
### Aliases
alias cp='cp -i'                      # Confirm before overwriting something
alias mv='mv -i'                      # Confirm bevore overwriting something
alias now='date +%Y-%m-%dT%H:%M%z'
alias ll="ls -lh"
alias lla="ls -lha --group-directories-first"
alias llt="ls -laht"                  # Time
alias lls="ls -lahS"                  # Size
alias dus="du -ckhs * | sort -hr"     # Size and sort
alias printpath='echo $PATH | tr ":" "\n"'

## Completions
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
  fi
fi

### GNU Pass
if [ -d "$HOME/Repos/pass" ]; then
    export PASSWORD_STORE_DIR="$HOME/Repos/pass"
fi

## Additional packages
if [ -d "$HOME/.dots/bin" ]; then
    PATH="$HOME/.dots/bin:$PATH"
fi

[ -r "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"

if [ -d "$HOME/.pyenv/bin" ]; then
    PATH="HOME/.pyenv/bin:$PATH"
    export PYENV_ROOT="$HOME/.pyenv"
    command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
    export PIPX_DEFAULT_PYTHON=$HOME/.pyenv/versions/3.10.9/bin/python
fi

if [ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.bash ]; then
    source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.bash
fi

command -v bat >&2 /dev/null && export MANPAGER="sh -c 'col -bx | bat -l man -p'"
