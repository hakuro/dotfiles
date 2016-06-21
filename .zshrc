# Language and Charset
export LC_ALL=ja_JP.UTF-8

#Interprreting Color
export LSCOLORS=gxfxcxdxbxegedabagacad
export LS_COLORS='di=36:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
case ${OSTYPE} in
    darwin*)
        alias ls="ls -G"
        ;;
    linux*)
        alias ls="ls --color"
        ;;
esac

# Lines configured by zsh-newuser-install
HISTFILE=$HOME/.zsh-history
HISTSIZE=1000000
SAVEHIST=1000000
bindkey -e

# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '$HOME/.zshrc'
zstyle ':completion:*' list-colors 'di=36' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Limit coredumpsize
limit coredumpsize 102400
# Display CRLF at end of row
unsetopt promptcr
# Use color prompt
setopt prompt_subst

# internal command: set jobs output to jobs -l by default
setopt long_list_jobs
# mark file type at complement list
setopt list_types
# resume suspend proccess if execute same command
setopt auto_resume
# do not send HUP signals to background jobs when shell exited
setopt NO_hup

# append hisytory file when using multiple zsh in same time
setopt append_history
# do not append to history file previous same command
setopt hist_ignore_dups
# write zsh start, exit time to history time
setopt extended_history
# edit history command before execute
setopt hist_verify
# share history
setopt share_history
# do not append the command start by space
setopt hist_ignore_space
# ignore history (fc -l) command from history list
setopt hist_no_store

# show complement list automatically
setopt auto_list
# toggle completions by TAB key
setopt auto_menu
# complement parentheses, etc automatically
setopt auto_param_keys
# add / to end of directory name automatically
setopt auto_param_slash
# spell check
setopt correct

# pushd automatically when cd
setopt auto_pushd
# do not pushd same directory
setopt pushd_ignore_dups
# handle #, ~, ^ as regexp in filename
setopt extended_glob
# expand =command to command pathname
setopt equals
# complement after =, etc in --prefix=/usr
setopt magic_equal_subst
# sort filename expand by numerical not lexically
setopt numeric_glob_sort
# allow 8bit output
setopt print_eight_bit
# enable cursor selection of complementions
zstyle ':completion:*:default' menu select=1
# cd by directory name
setopt auto_cd
# enable function of expand {a-c} to a b c
setopt brace_ccl
# unable to use flow control by Ctrl+S/Ctrl+Q
setopt NO_flow_control
# accept # as comment at commandline
setopt interactive_comments
# append / to the end of filename when complementions matched to directory name
setopt mark_dirs
# show complementions closely
setopt list_packed
# do not erase the end of / automatically
setopt noautoremoveslash
# do not beep when display complementions
setopt nolistbeep
# enable to use simplified sentence such as for, repeat, select, if, function
setopt short_loops

# prompt settings
autoload colors
colors
PROMPT="%{${fg[green]}%}%n@%m:%(!.#.$) %{${reset_color}%}"
PROMPT2="%{${fg[green]}%}%_> %{${reset_color}%}"
SPROMPT="%{${fg[red]}%}correct: %R -> %r [nyae]? %{${reset_color}%}"
RPROMPT="%{${fg[blue]}%}[%~]%{${reset_color}%}"

#################################################################################

# terminal
export TERM=xterm-256color

# command history search settings
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# pyenv settings
if [[ -s $HOME/.pyenv ]]; then
    export PATH="$HOME/.pyenv/bin:$PATH"
    eval "$(pyenv init -)"
fi

# sqlplus settings
if [[ -s /usr/local/sqlplus/instantclient_11_2 ]]; then
    export ORACLE_HOME=/usr/local/sqlplus/instantclient_11_2
    export PATH=$ORACLE_HOME:$PATH
    export DYLD_LIBRARY_PATH=$ORACLE_HOME
#    export NLS_LANG=American_America.AL32UTF8
    export NLS_LANG=Japanese_Japan.AL32UTF8
fi

# AWS CLI settings
if [[ -s /usr/local/aws/bin/aws_zsh_completer.sh ]]; then
    export PATH="/usr/local/aws/bin:$PATH"
    source /usr/local/aws/bin/aws_zsh_completer.sh
fi

# shortened emacs command
alias e="emacs"

