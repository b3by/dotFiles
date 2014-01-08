#!/bin/bash

# stuff
STAT=/usr/local/bin/vnstat
BREW=`command -v brew`
IOREG=`command -v ioreg`

# colors, like in gay bacon strips
bluecolor="\033[1;34m"
greencolor="\033[1;32m"
redcolor="\033[1;31m"
gaycolor="\033[1;35m"
yellowcolor="\033[1;33m"
lightblue="\033[1;36m"
nocolor="\033[0m"
whitecolor="\033[1;37m"

# underline
under="\033[4m"
nounder="\033[0m"

print_uptime() {
    if [ $# -eq 0 ]; then
        echo -e "${bluecolor}`uptime | sed -E 's/^.{10}//' | sed -E 's/, [0-9][0-9]? user(s)?,.*//g'`${nocolor}"
    else
        echo -e "☕ uptime is ${bluecolor}`uptime | sed -E 's/^.{10}//' | sed -E 's/, [0-9][0-9]? user(s)?,.*//g'`${nocolor}"
    fi
}

print_battery() {
    if [ ! -x $IOREG ]; then
        echo "battery: cannot execute ioreg"
    else
        ioreg -l | echo `grep "BatteryInfo\" ="` | {
            read line
            CYCLECOUNT=`echo $line | sed -e s/.*$CycleCount=//g -e s/\}.*//g`
            if [ $# -eq 0 ]; then
                echo -e "${bluecolor}$CYCLECOUNT${nocolor}"
            else
                echo -e "🍺 battery is ${bluecolor}$CYCLECOUNT${nocolor} cycles old"
            fi
        }
    fi
}

print_weather() {
    WEATHER=`curl --silent "http://xml.weather.yahoo.com/forecastrss?p=ITXX0052&u=c" | grep -E '(Current Conditions:|C<BR)' | tail -n 1 | sed '$s/......$//' | tr '[A-Z]' '[a-z]'`
    if [ $# -eq 0 ]; then
        echo -e "$WEATHER"
    else
        echo -e "🔆 weather in Naples: $WEATHER"
    fi
}

print_ethernet_info() {
    if [ ! -x $STAT ]; then
        echo "network: cannot find $STAT"
    else
        ETHLINE=`ipconfig getifaddr en0`
        if [ -n "$ETHLINE" ]; then
            echo -e "(en0) ${greencolor}up${nocolor} on ${under}$ETHLINE${nounder}"
            $STAT -u
            $STAT -d -i en0 | tail -3 | head -1 | awk '{print "   wire tx → " $5$6"\n   wire rx → "$2$3}'
        else
            echo -e "(en0) ${redcolor}down${nocolor}"
        fi
    fi
}

print_wireless_info() {
    if [ ! -x $STAT ]; then
        echo "network: cannot find $STAT"
    else
        WLNLINE=`ipconfig getifaddr en1`
        if [ -n "$WLNLINE" ]; then
            echo -e "(en1) ${greencolor}up${nocolor} on ${under}$WLNLINE${nounder}"
            $STAT -u
            $STAT -d -i en1 | tail -3 | head -1 | awk '{print "   wlan tx → " $5$6"\n   wlan rx → "$2$3}'
        else
            echo -e "(en1) ${redcolor}down${nocolor}"
        fi
    fi
}

print_external_address() {
    EXTLINE=`dig +short myip.opendns.com @resolver1.opendns.com`
    if [ -n "$EXTLINE" ]; then
        echo -e "${greycolor}(ext) ${greencolor}up${nocolor} on ${under}$EXTLINE${nounder}"
    else
        echo -e "(ext) ${redcolor}not set${nocolor}"
    fi
}

print_df_numbers() {
    df -h / | grep "/dev/" | awk '{print "on /       \033[32m" $4"\033[00m are free (~"(100-$5)"%)"}'
    df -h /Users | grep "Users" | awk '{print "on " $9" \033[32m"$4 "\033[00m are free (~"(100-$5)"%)"}'
}

print_git_status() {
    cd /Users/b3by/;
    GITFOLDER=.git/
    if [ ! -x $GITFOLDER ]; then
        echo -e "(home ${bluecolor}repo${nocolor}) git repo ${redcolor}not set!${nocolor}"
    else
        GITSTATUS=`git status | egrep 'modified|new' | grep -v finder.plist`
        GITBRANCH=`git branch | awk '{print $2}'`
        if [ -n "$GITSTATUS" ]; then
            echo -e "(home ${bluecolor}repo${nocolor}) ${redcolor}to commit${nocolor} on ${gaycolor}$GITBRANCH${nocolor}"
            echo "$GITSTATUS"
        else
            echo -e "(home ${bluecolor}repo${nocolor}) ${gaycolor}$GITBRANCH${nocolor} ${greencolor}clean${nocolor}"
        fi
    fi
}

print_brew_status() {
    if [ ! -x $BREW ]; then
        echo -e "(home ${bluecolor}brew${nocolor}) ${redcolor}not installed!${nocolor}"
    else
        BREWSTATUS=`brew outdated`
        if [ -z $BREWSTATUS ]; then
            echo -e "(home ${bluecolor}brew${nocolor}) nothing to update"
        else
            echo "(home ${bluecolor}brew${nocolor}) something new"
        fi
    fi
}

print_drives() {
    mount | grep /Volumes | awk '{printf "(home \033[34mmnts\033[00m) " substr($3, 9, 20) " mounted\n", 27 }'
}

print_mail() {
    
    USERNAME="b3by.in.th3.sky@gmail.com"
    PASSWORD=`security 2>&1 find-generic-password -g -a gmail | grep password | awk -F '"' '{print $2}'`
        
    EMAIL=`curl -u $USERNAME:$PASSWORD --silent "https://mail.google.com/mail/feed/atom" | tr -d '\n' | awk -F '<entry>' '{for (i=2; i<=NF; i++) {print $i}}' | sed -n "s/<title>\(.*\)<\/title.*name>\(.*\)<\/name>.*/\2 - \1/p"`
 
    if [ -n "$EMAIL" ]; then
        NUM=`echo "$EMAIL" | wc -l | awk '{printf $1}'`
        echo -e "(home ${bluecolor}mail${nocolor}) ${redcolor}$NUM${nocolor} new in inbox"
        
        IFS=$'\n'
        for i in $EMAIL
        do
            len=${#i}
            if [ "$len" -gt 24 ]; then
                echo "      "${i:0:22}"..."
            else
                echo "      "$i
            fi
        done
    else
        echo -e "(home ${bluecolor}mail${nocolor}) inbox  ${greencolor}clean${nocolor}"
    fi
}

echo ""
echo ""
echo `date "+%A, %d %b %Y"`
echo `date "+%r"`
echo -e "(${bluecolor}weather${nocolor}) `print_weather`"
echo ""
echo -e "general summary"
echo -e "${lightblue}-------------------------------"
echo -e "${nocolor}system uptime is `print_uptime`"
echo "battery cycles count is on `print_battery`"
print_df_numbers
echo -e "${lightblue}-------------------------------"
echo ""
echo -e "${nocolor}network summary"
echo -e "${lightblue}-------------------------------${nocolor}"
print_ethernet_info
print_wireless_info
print_external_address
echo -e "${lightblue}-------------------------------${nocolor}"
echo ""
echo "stuff"
echo -e "${lightblue}-------------------------------${nocolor}"
print_drives
# print_brew_status
print_git_status
print_mail
echo -e "${lightblue}-------------------------------${nocolor}"
