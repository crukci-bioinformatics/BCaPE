#!/bin/bash
chmod ugo+r bcape.sqlite analytics.js
mkdir -p logs
chmod ugo+rwx logs
docker run -u shiny -d --rm -p 80:3838 \
  -v ${PWD}/bcape.sqlite:/srv/shiny-server/bcape/bcape.sqlite \
  -v ${PWD}/analytics.js:/srv/shiny-server/bcape/analytics.js \
  -v ${PWD}/logs:/var/log/shiny-server \
  crukci-bioinformatics/bcape
