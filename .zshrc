# Set up the prompt

#autoload -Uz promptinit
#promptinit
#prompt adam1
autoload -U colors && colors
PROMPT="%{$fg_bold[red]%}%n%{$fg_bold[black]%} at %{$fg_bold[green]%}%m%k %B%{$fg_no_bold[yellow]%}%(4~|...|)%3~%{$fg_bold[black]%} %# %b%f%k"

setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
#eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

setopt hist_ignore_space

if [[ -n $STY || -n $TMUX ]]; then
    function title() { print -Pn "\ek$1\e\\"}
    function precmd() { title "%20<..<%~%<<" }
    function preexec() { title "%20>..>$1%<<" }
#     export PS1="%{${fg[cyan]}%}[%D{%H:%M} %20<..<%~%<<]%{$reset_color%} "
# else
#     export PS1="%{${fg[cyan]}%}[%D{%H:%M} %n@%m:%20<..<%~%<<]%{$reset_color%} "
fi

if [[ `uname` == 'Linux' ]]; then
    alias ls='ls --color'
else
    alias ls='ls -G'
fi
alias ll='ls -l'
alias tat='tmux attach-session -t'
alias b='cd $OLDPWD'
alias e="emacsclient"

# function precmd() {
#     case "$TERM" in
# 	screen | screen.rxvt | *xterm*)
# 	    print -Pn "\ek%-3~\e\\"
# 	    ;;
#     esac
# }

#export KLEEBASE=/home/styx/klee-build/klee
#export LLVMSRC=/home/styx/klee-build/llvm
#export LLVMOBJ=/home/styx/klee-build/llvm-build
#export PATH=$KLEEBASE/Release+Asserts/bin:$LLVMOBJ/Release+Asserts/bin:$PATH

# set up fasd
if hash fasd 2>/dev/null; then
    eval "$(fasd --init auto)"
fi

# set up rbenv
if hash rbenv 2>/dev/null; then
    eval "$(rbenv init -)"
fi

# set up perlbrew
if [[ -d "$HOME/.pb" ]]; then
    export PERLBREW_ROOT=~/.pb
    source ~/.pb/etc/bashrc
fi
