#!/bin/bash
# sharad-start.sh

. ./sharad-usage.sh
start() {
    while getopts ":h" arg; do
      case ${arg} in
        h)
          usage
          exit 0
          ;;
        :)
          echo "$0: Must supply an argument to -$OPTARG." >&2
          exit 1
          ;;
        ?)
          echo "Unrecognised option: -$OPTARG." >&2
          exit 1
          ;;
      esac
    done
    
    potential_exe=$(stat -c "%n" * | grep sharad-backend)
    
    if [[ -z $potential_exe ]]; then
            echo "Unable to find a file corresponding to the sharad server executable. This script must be run in a folder containing an executable named sharad-backend*" >&2
            exit 1
    fi
    
    if [[ $(echo $potential_exe | wc -w) != 1  ]]; then
            echo -e "Could not determine which sharad server executable to run. Found:\n"
            echo "$potential_exe" >&2
            exit 1
    fi
    
    "./$potential_exe"
}
