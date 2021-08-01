#!/bin/bash
# sharad-usage.sh

function usage {
        echo "USAGE: sharad [COMMAND] [OPTIONS]"
        echo -e "Wrapper around the sharad web application. Its role is to control the starting and stopping of the application, passing options to the server\n"
        echo "Options:"
        echo -e "  -h    Displays this usage\n"
        echo "Commands:"
        echo "  start    Starts the sharad server"
}

