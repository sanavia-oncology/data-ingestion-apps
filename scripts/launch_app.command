#!/usr/bin/env zsh
trap 'kill $(jobs -p) 2>/dev/null' INT TERM EXIT

source ~/.env_data_ingestion_apps

R -e 'shiny::runApp('$QC_APP_DIR', port=3001, launch.browser=FALSE)' &
R -e 'shiny::runApp('$FC_APP_DIR', port=3002, launch.browser=FALSE)' &
R -e 'shiny::runApp('$UL_APP_DIR', port=3003, launch.browser=FALSE)' &

wait