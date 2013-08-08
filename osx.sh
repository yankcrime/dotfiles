#!/bin/sh
# Inspiration and more here: https://github.com/mathiasbynens/dotfiles/blob/master/.osx

# Skip DMG verification
defaults write com.apple.frameworks.diskimages skip-verify true

# Disable opening and closing window animation
defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false

# Enable subpixel font rendering on non-Apple LCDs
# defaults write NSGlobalDomain AppleFontSmoothing -int 2
# value of 1 seems to look better on my Dell U2311h
defaults write NSGlobalDomain AppleFontSmoothing -int 1

# Finder: disable window animations and Get Info animations
defaults write com.apple.finder DisableAllAnimations -bool true

# Avoid creating .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Speed up Mission Control animations
defaults write com.apple.dock expose-animation-duration -float 0.2

# Completely disable Dashboard
defaults write com.apple.dashboard mcx-disabled -boolean YES

# Remove useless icons from Safari’s bookmarks bar
defaults write com.apple.Safari ProxiesInBookmarksBar "()"

# Enable the Develop menu and the Web Inspector in Safari
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari "com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled" -bool true

# Add a context menu item for showing the Web Inspector in web views
defaults write NSGlobalDomain WebKitDeveloperExtras -bool true

# Make the iTunes arrow links go to your library instead of the iTunes Store
defaults write com.apple.iTunes invertStoreLinks -bool true

# Disable the Ping sidebar in iTunes
defaults write com.apple.iTunes disablePingSidebar -bool true

# Disable all the other Ping stuff in iTunes
defaults write com.apple.iTunes disablePing -bool true

# Make ⌘ + F focus the search input in iTunes
defaults write com.apple.iTunes NSUserKeyEquivalents -dict-add "Target Search Field" "@F"

# Disable send and reply animations in Mail.app
defaults write com.apple.Mail DisableReplyAnimations -bool true
defaults write com.apple.Mail DisableSendAnimations -bool true
defaults write com.apple.mail DisableReplyAnimations -bool TRUE
defaults write com.apple.mail DisableSendAnimations -bool TRUE

# Set the icon size of Dock items to 36 pixels
defaults write com.apple.dock tilesize -int 36

echo "Now reboot!"
