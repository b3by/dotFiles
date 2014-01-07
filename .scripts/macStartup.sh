#!/bin/bash

# If everything goes wrong... go left!
# This bunch of commands restores my Mac to a
# non-OhMyGodIWantToDie configuration

# disable dashboard
# to re-enable, set it to NO
echo "Disabling dashboard..."
defaults write com.apple.dashboard mcx-disabled -boolean YES
killall Dock
echo "Done."

# show path in finder
# to disable, set it to NO
echo "Enabling path in Finder..."
defaults write com.apple.finder _FXShowPosixPathInTitle -bool YES
killall Finder
echo "Done."

# wake deep-sleep up from death
# to prevent deep-sleep, set it to 3
echo "Fixing deep-sleep death..."
sudo pmset -a hibernatemode 25
echo "Done."

# disable wake up on lid opening or AC plugging
# to enable, set them to 1
echo "Fixing wake up on lid opening and stuff..."
sudo pmset -a lidwake 0
sudo pmset -a acwake 0
echo "Done."

# install brew
echo "Installing brew..."
ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"
echo "Done."

# some brew installation
brew install wget
brew install vnstat
brew install clisp
# brew install ettercap
brew install nmap
brew install arp-scan
brew install proxychains-ng
# brew install wireshark
# brew install gti
# brew install sl

# install texlive
wget -O MacTex.pkg -c "http://ctan.mirror.garr.it/mirrors/CTAN/systems/mac/mactex/MacTeX.pkg"
sudo installer -pkg MacTex.pkg -target /
rm MacTex.pkg

# install heroku toolbelt
wget -O heroku-toolbelt.pkg -c "http://assets.heroku.com/heroku-toolbelt/heroku-toolbelt.pkg"
sudo installer -pkg heroku-toolbelt.pkg -target /
rm heroku-toolbelt.pkg
