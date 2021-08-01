#!/bin/bash
# sharad.sh

# This script is meant to be a wrapper around the sharad web application.
# Its role is to control the starting and stopping of the application, passing options to the server

. ./sharad-usage.sh
if [[ $1 != "start" ]]; then
        echo "Unknown command $1" >&2
        usage >&2
        exit 1
fi

shift
./sharad-start.sh "$@"

