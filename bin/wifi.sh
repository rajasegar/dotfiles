#!/bin/bash

WIFI_ACTIVE='☘︎'
WIFI_INACTIVE=''

status=`ifconfig en0 | awk '/status:/{print $2}'`

if [[ $status -eq "active" ]]; then
  echo -n " #[fg=color27]$WIFI_ACTIVE "
else
  echo -n " #[fg=color27]WIFI: $WIFI_INACTIVE "
fi
