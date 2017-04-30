#!/bin/bash

parse_essid(){
        grep ESSID | sed -e "s/ESSID://g" -e "s/\"//g"
}

parse_bssid(){
        grep Address | sed -e "s/Cell [0-9]\+ \- Address: //g"
}


parse_freq(){
        grep Frequency | sed -e "s/Frequency://g" -e "s/ GHz (Channel .*//g"
}


parse_channel(){
        grep "^Channel" | sed -e "s/Channel://g"
}


parse_rssi(){
        grep "Signal level" | sed -e "s/.\+ Signal level=//g" -e "s/ dBm//g"
}


parse_quality(){
        grep "Quality=" | sed -e "s/Quality=//g" -e "s/  Signal level=.\+//g" | bc -l | sed -e "s/^\./0\./g"
}




if [ $UID -ne 0 ]; then
        echo "root  privilege required"
        exit 1
fi

echo -e "\n#########################################################"
echo -e "##### SELECT WIRELESS NETWORK INTERFACE FROM BELLOW #####"
echo -e "#########################################################"
ip a | grep "^[0-9]: "
echo -e "#########################################################"
echo -e "#########################################################"

echo -n "interface?: "
read NIC

EXISTS=`ip a | grep " $NIC:"`

if [ -z "$EXISTS" ] || [ -z "$NIC" ]; then
        echo "your interface does not exist"
        exit 1
fi

DIR=""
while [ ! -d "$DIR" ]; do
        echo -n "where to save?: "
        read DIR
done
DIR=`echo "$DIR" | sed -e "s/\/$//g"`


INTERVAL=""
while [ -z `echo "$INTERVAL" | grep "^[0-9]\+$"` ]; do
        echo -n "interval?: "
        read INTERVAL 
done


TIMES=""
while [ -z `echo "$TIMES" | grep "^[0-9]\+$"` ]; do
        echo -n "how many times do you measure?: "
        read TIMES
done

cd $DIR

for i in `seq 1 $TIMES`; do
        rawdata=`sudo iwlist $NIC scan`
        if [ $? -ne 0 ]; then
                echo "iwlist failed... sleep in some time"
                sleep 10
                continue
        fi
        data=`echo "$rawdata" | grep -A 5 Cell | sed -e "s/^ *//g"`
        now=`date +%s`
        essid=`echo "$data" | parse_essid`
        bssid=`echo "$data" | parse_bssid`
        freq=`echo "$data" | parse_freq`
        channel=`echo "$data" | parse_channel`
        rssi=`echo "$data" | parse_rssi`
        quality=`echo "$data" | parse_quality`
        columns=`echo "$essid" | wc -l`
        for j in `seq 1 $columns`; do
                line="`echo $essid | cut -d " " -f$j`,`echo $bssid | cut -d " " -f$j`,`echo $freq | cut -d " " -f$j`,`echo $channel | cut -d " " -f$j`,`echo $rssi | cut -d " " -f$j`,`echo $quality | cut -d " " -f$j`"
                echo $line >> "SCAN_at_$now.csv"
        done
        sleep $INTERVAL
done

