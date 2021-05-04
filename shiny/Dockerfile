FROM crukcibioinformatics/shiny-base

MAINTAINER Matt Eldridge "bcape@cruk.cam.ac.uk"

RUN R -e 'install.packages("RSQLite", repos = "https://cloud.r-project.org")'
RUN R -e 'install.packages("dbplyr", repos = "https://cloud.r-project.org")'
RUN R -e 'install.packages("highcharter", repos = "https://cloud.r-project.org")'

RUN mkdir /srv/shiny-server/bcape
RUN mkdir /srv/shiny-server/bcape/www

RUN chmod -R ugo+rx /srv/shiny-server/bcape

COPY *.R /srv/shiny-server/bcape/
COPY bcape.sqlite /srv/shiny-server/bcape/
COPY analytics.js /srv/shiny-server/bcape/
COPY www/* /srv/shiny-server/bcape/www/

RUN chmod -R ugo+r /srv/shiny-server/bcape

