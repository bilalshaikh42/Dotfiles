#if not running interactively, dont do anything
[ -z "$PS1" ] && return


#Set directory for dot files
DOTFILES_DIR="$HOME/.dotfiles"


#Source the system dotfiles

for DOTFILE in "$DOTFILES_DIR"/system/.{function,function_*,env,alias,path,completion,grep,prompt,custom}; do
    [ -f "$DOTFILE" ] && . "$DOTFILE"

done


#Set file system colors

eval "$(dircolors "$DOTFILES_DIR"/system/.dir_colors)"



#display settings for BASH On windows
if uname -r | grep 'Microsoft' -q
then
    export DISPLAY=localhost:0.0
    echo Bash on Windows detected. Set DIPLAY to $DISPLAY
    export DOCKER_HOST=tcp://localhost:2375
    echo Set Docker_Host to localhost:2375. Enable port on docker daemon on windows
fi

#export
export DOTFILESS_DIR

umask 007
