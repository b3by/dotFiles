############################
# A PS1 inspired by Gentoo #
############################
if [[ ${EUID} == 0 ]] ; then
	PS1='\[\033[01;31m\]\h\[\033[01;34m\] \W \$\[\033[00m\] '
else
	PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] '
fi
###########################

MAILADDR="b3by.in.th3.sky@gmail.com"
PATH="$PATH:~/bin/"

###########################

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# web methods from @janmoesen's
for method in GET HEAD POST PUT DELETE; do
  alias "$method"="lwp-request -m '$method'"
done

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

source $HOME/.aliases
