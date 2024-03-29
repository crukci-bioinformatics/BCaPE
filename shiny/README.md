BCaPE Shiny app
===============

Web application for the Breast Cancer PDTX Encyclopaedia built using the
[R Shiny framework](https://www.rstudio.com/products/shiny).

The data for the Shiny app are contained in an [SQLite](https://www.sqlite.org)
database file named `bcape.sqlite`. The SQLite database within this folder is
empty but contains the schema definitions for all tables and indexes. See the
[processing](../processing) folder for details on how to populate the database.

### R package dependencies

The application depends on the following R packages which can be installed
using the R `install.packages` function.

- [tidyr](http://tidyr.tidyverse.org)
- [dplyr](http://dplyr.tidyverse.org)
- [dbplyr](https://cran.r-project.org/web/packages/dbplyr/index.html)
- [RSQLite](https://cran.r-project.org/web/packages/RSQLite/index.html)
- [shiny](https://cran.r-project.org/web/packages/shiny/index.html)
- [DT](https://rstudio.github.io/DT)
- [highcharter](http://jkunst.com/highcharter)

### Running the application standalone

To run the Shiny app standalone:

```
Rscript start_shiny_app.R
```

The application should be launched in a new browser window.

### Deployment within Shiny Server

Alternatively, to run within [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server),
copy the R code (files ending with '.R'), the database (`bcape.sqlite`), the analytics JavaScript
function (`analytics.js`) and the images within the `www` subdirectory to a new directory named
`bcape` within the Shiny server site directory, usually `/srv/shiny-server`. The application should
be available at http://host:port/bcape, where `host` and `port` need to be substituted with the name
of the host on which Shiny Server is running and the port number on which it is configured to listen
(see `/etc/shiny-server/shiny-server.conf`).

### Deployment using Docker

This folder contains a Dockerfile used to build a [Docker](https://www.docker.com) image in
which Shiny Server, R, the BCaPE web application and all its R package dependencies are installed.
The Docker image is built on the [shiny-base](https://github.com/crukci-bioinformatics/shiny-base)
image which packages Shiny Server, R and the [tidyverse](https://www.tidyverse.org)
R packages.

The Docker image is available on [Docker Hub](https://hub.docker.com/r/crukcibioinformatics/bcape)
and can be retrieved as follows:

```
docker pull crukcibioinformatics/bcape
```

To deploy the application within a Docker container running Shiny Server:

```
chmod ugo+r bcape.sqlite
mkdir -p logs
chmod ugo+rwx logs
docker run -u shiny -d -p 3838:3838 \
  -v ${PWD}/bcape.sqlite:/srv/shiny-server/bcape/bcape.sqlite \
  -v ${PWD}/logs:/var/log/shiny-server \
  crukcibioinformatics/bcape
```

Note that this replaces the empty database contained within the Docker
image with the `bcape.sqlite` file in the current directory on the host
file system. Likewise, a log directory is created and bound into the
container so that logging information is accessible outside the container.

Similarly we could replace the Shiny Server configuration to change some
configuration settings by mounting an external configuration file as
`/etc/shiny-server/shiny-server.conf` within the container. See the
[shiny-base](https://github.com/crukci-bioinformatics/shiny-base/blob/master/README.md)
Docker image for more details on how to configure Shiny Server in this way.

The application should be accessible at http://localhost:3838/bcape.

By default Shiny Server listens on port 3838. This can be remapped to another
port using Docker with the `-p` option.

### Deployment using Singularity

The [BCaPE site](http://caldaslab.cruk.cam.ac.uk/bcape) is deployed using
[Singularity](http://singularity.lbl.gov), which is another container system.
A Singularity image, in which Shiny Server, R, the BCaPE Shiny application and
its R package dependencies are installed, can be built from the Docker image
available on Docker Hub as follows:

```
singularity build bcape.sif docker://crukcibioinformatics/bcape
```

This does not require Docker to be installed.

The image can be run as follows:

```
mkdir -p logs
singularity run \
  -B bcape.sqlite:/srv/shiny-server/bcape/bcape.sqlite \
  -B logs:/var/log/shiny-server \
  bcape.sif
```

### Google analytics

Usage of the application can be monitored using
[Google Analytics](https://www.google.com/analytics).
For this an account on Google Analytics will need to be set up and a
tracking ID obtained.

The following JavaScript function should be added to `analytics.js`:

```
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-12345678-1', 'auto');
  ga('send', 'pageview');

  $(document).on('shiny:value', function(event) {
    ga('send', 'event', 'update', event.name);
  });
```

where the tracking ID `UA-12345678-1` should be substituted with the one to be
used for the deployed application.

If deploying with Docker, analytics.js can be mounted within the container
as follows:

```
docker run -u shiny -d -p 3838:3838 \
  -v ${PWD}/bcape.sqlite:/srv/shiny-server/bcape/bcape.sqlite \
  -v ${PWD}/analytics.js:/srv/shiny-server/bcape/analytics.js \
  -v ${PWD}/logs:/var/log/shiny-server \
  crukcibioinformatics/bcape
```

Similarly with Singularity:

```
singularity run \
  -B bcape.sqlite:/srv/shiny-server/bcape/bcape.sqlite \
  -B analytics.js:/srv/shiny-server/bcape/analytics.js \
  -B logs:/var/log/shiny-server \
  bcape.sif
```

