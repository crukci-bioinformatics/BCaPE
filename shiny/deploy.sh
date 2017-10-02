#!/bin/bash
docker run -u shiny -d --rm -p 80:3838 -v ${PWD}/data:/srv/shiny-server/bcape/data -v ${PWD}/analytics:/srv/shiny-server/bcape/analytics -v ${PWD}/logs:/var/log/shiny-server crukci-bioinformatics/bcape
