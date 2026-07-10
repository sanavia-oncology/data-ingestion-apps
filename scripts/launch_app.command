#!/usr/bin/env zsh
trap 'kill $(jobs -p) 2>/dev/null' INT TERM EXIT

source ~/.env_data_ingestion_apps

APP_DIR="$QC_APP_DIR" R -e 'shiny::runApp(Sys.getenv("APP_DIR"), port=3001, launch.browser=FALSE)' &
APP_DIR="$FC_APP_DIR" R -e 'shiny::runApp(Sys.getenv("APP_DIR"), port=3002, launch.browser=FALSE)' &
APP_DIR="$UL_APP_DIR" R -e 'shiny::runApp(Sys.getenv("APP_DIR"), port=3003, launch.browser=FALSE)' &
APP_DIR="$GA_APP_DIR" R -e 'shiny::runApp(Sys.getenv("APP_DIR"), port=3004, launch.browser=FALSE)' &

wait
