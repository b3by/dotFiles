#!/bin/bash

# If everything goes wrong... go left! This bunch of commands restores my Mac to
# a non-ohMyGodIWantToDie configuration.

function startTask () {
    echo "$(tput setaf 2)*** $1$(tput sgr0)"
}

function doneTask () {
    echo "$(tput setaf 2)*** Done.$(tput sgr0)"
}

function infoMessage () {
    echo "$(tput setaf 6)*** $1$(tput sgr0)"
}

function checkAndInstallFromBrew () {
    if [ -z `brew list | grep -w $1` ]; then
        startTask "$1 not installed. Installing..."
        brew install $1 $2 $3 $4
        doneTask
    else
        infoMessage "$1 already installed. Skipping..."
    fi
}

function checkAndInstallFromCask() {
    if [ -z `brew cask list | grep -w $1` ]; then
        startTask "$1 not installed. Installing..."
        brew cask install $1 $2 $3 $4
        doneTask
    else
        infoMessage "$1 already installed. Skipping..."
    fi
}

# change hostname
startTask "Please specify hostname:"
read -r response
if [ -z $response ]; then
    sudoscutil --set HostName $response
else
    infoMessage "Skipping..."
fi

# disable dashboard
# to re-enable, set it to NO
startTask "Disable dashboard? (y/n)"
read -r response
case $response in
    [yY])
        defaults write com.apple.dashboard mcx-disabled -boolean YES
        killall Dock
        doneTask ;;
    *)  ;;
esac

# set the dock size to 32px and magnification size to 64px
startTask "Set dock size to 32px and magnification size to 64px? (y/n)"
read -r response
case $response in
    [yY])
        defaults write com.apple.dock tilesize -int 32
        defaults write com.apple.dock largesize -int 64
        killall Dock
        doneTask ;;
    *)  ;;
esac

# show path in finder
# to disable, set it to NO
startTask "Enable path in Finder? (y/n)"
read -r response
case $response in
    [yY])
        defaults write com.apple.finder _FXShowPosixPathInTitle -bool YES
        killall Finder
        doneTask ;;
    *) ;;
esac

# wake deep-sleep up from death
# to prevent deep-sleep, set it to 3
startTask "Fixing deep-sleep death"
sudo pmset -a hibernatemode 25
doneTask

# disable wake up on lid opening or AC plugging
# to enable, set them to 1
startTask "Fixing wake up on lid opening and stuff..."
sudo pmset -a lidwake 0
sudo pmset -a acwake 0
doneTask

startTask "Disabling keyboard backlight when mac is not used for 5 minutes? (y/n)"
read -r response
case $response in
    [yY])
        defaults write com.apple.BezelServices kDimTime -int 300 ;;
    *) ;;
esac

startTask "Requiring password immediately after sleep or screen saver begins? (y/n)"
read -r response
case $response in
    [yY])
        defaults write com.apple.screensaver askForPassword -int 1
        defaults write com.apple.screensaver askForPasswordDelay -int 0 ;;
    *) ;;
esac

startTask "Show all file extensions by default? (y/n)"
read -r response
case $response in
    [yY])
        defaults write NSGlobalDomain AppleShowAllExtensions -bool true ;;
    *) ;;
esac


startTask "Enable snap-to-grid everywhere? (y/n)"
read -r response
case $response in
    [yY])
        /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
        /usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
        /usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist ;;
    *) ;;
esac

startTask "Prevent Time Machine from prompting to use new hard drives as backup volume? (y/n)"
read -r response
case $response in
    [yY])
        defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true ;;
    *) ;;
esac

# check for brew installation
if ! type brew > /dev/null; then
    startTask "Brew not found. Installing..."
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    doneTask
else
    infoMessage "Homebrew already installed. Skipping..."
fi

# some brew installations
checkAndInstallFromBrew caskroom/cask/brew-cask
checkAndInstallFromBrew node
checkAndInstallFromBrew wget
checkAndInstallFromBrew vnstat
checkAndInstallFromBrew clisp
# checkAndInstallFromBrew ettercap
checkAndInstallFromBrew nmap
checkAndInstallFromBrew arp-scan
checkAndInstallFromBrew proxychains-ng
checkAndInstallFromBrew emacs --cocoa --HEAD
checkAndInstallFromBrew tmux
checkAndInstallFromBrew reattach-to-user-namespace
checkAndInstallFromBrew gti
checkAndInstallFromBrew sl

# some cask installations
checkAndInstallFromCask google-chrome
checkAndInstallFromCask emacs
checkAndInstallFromCask vlc
checkAndInstallFromCask skype
checkAndInstallFromCask iterm2
checkAndInstallFromCask spotify
checkAndInstallFromCask transmission
checkAndInstallFromCask dropbox
checkAndInstallFromCask menumeters
checkAndInstallFromCask caffeine

# install texlive
if ! type latex > /dev/null; then
    startTask "TexLive not installed."
    infoMessage "Downloading MacTex package..."
    wget -O MacTex.pkg -c "http://ctan.mirror.garr.it/mirrors/CTAN/systems/mac/mactex/MacTeX.pkg"
    startTask "Installing TexLive..."
    sudo installer -pkg MacTex.pkg -target /
    rm MacTex.pkg
    doneTask
else
    infoMessage "TexLive already installed. Skipping..."
fi

if [ ! -d ~/.oh-my-zsh ]; then
    infoMessage "oh-my-zsh not present."
    startTask "Cloning into oh-my-zsh repo..."
    git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
    doneTask
    startTask "Setting zsh as default shell..."
    chsh -s /bin/zsh
    doneTask
else
    infoMessage "on-my-zsh already present. Skipping..."
fi
