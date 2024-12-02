#!/bin/env bash

set -e

if [ "$#" != "1" ]; then
    echo "USAGE: $(basename $0) [start|stop|restart|status|test]"
    exit 2
fi

# NOTE: termux sets $PREFIX to it's root directory
export PIDFILE=$PREFIX/tmp/personal-webserver.pid
export LOGFILE=$PREFIX/tmp/personal-webserver.log
export WWW=$(dirname $0)/../

start() {
    if ! $(isrunning); then
        nohup python3 -m http.server 8080 -b 127.0.0.1 -d $WWW >$LOGFILE 2>&1 &
        echo $! > $PIDFILE
        echo "server started"
    else
        echo "server already running"
    fi
}

stop() {
    if $(isrunning); then
        kill -9 $(getpid)
        rm $PIDFILE
        echo "server stopped"
    else
        echo "server was not running"
    fi
}

getpid() {
    test -f $PIDFILE && cat $PIDFILE
}

isrunning() {
    pid=$(getpid)
    test -n "$pid" && ps --pid $pid >/dev/null
}


command=$1

if [ "$command" = "start" ]; then
    start
elif [ "$command" = "stop" ]; then
    stop
elif [ "$command" = "restart" ]; then
    stop
    start
elif [ "$command" = "status" ]; then
     if $(isrunning); then
         echo "server is running (PID=$(getpid))"
     else
         echo "server is not running"
         exit 1
     fi
elif [ "$command" = "test" ]; then
    isrunning
else
    echo "unknown command: $command"
    exit 2
fi
