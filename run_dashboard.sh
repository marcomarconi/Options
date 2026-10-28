#!/usr/bin/env bash
# Double-click launcher for the Shiny dashboard (Shiny/ui2.R).
# Opens the browser once the app is up; if it is already running, just opens the browser.
# Log: Shiny/dashboard.log
PORT=7425
URL="http://127.0.0.1:$PORT"
cd "$(dirname "$(readlink -f "$0")")/Shiny" || exit 1

if curl -s -o /dev/null "$URL"; then
    xdg-open "$URL"; exit 0
fi

Rscript -e "shiny::runApp(source('ui2.R', local = TRUE)\$value, port = $PORT, launch.browser = TRUE)" \
    > dashboard.log 2>&1
