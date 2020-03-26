FROM rocker/shiny:3.5.1
MAINTAINER Marine Institute
# install ssl
# and gdal
RUN sudo apt-get update && apt-get install -y libssl-dev libudunits2-0 libudunits2-dev libproj-dev libgdal-dev && apt-get clean && rm -rf /var/lib/apt/lists/ && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
Run Rscript -e "install.packages(c('dplyr','plotly','leaflet'),repos = 'https://cran.rstudio.com')"
RUN sudo chown -R shiny:shiny /var/lib/shiny-server/
RUN Rscript -e "install.packages(c('shiny','flexdashboard','shinydashboard','shinyjs','htmltools','shinycssloaders','reshape2','dplyr','ggridges','tidyr','tidyverse','DT'), repos='https://cran.rstudio.com/')" && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

COPY www /srv/shiny-server/igfstest/www
COPY Data /srv/shiny-server/igfstest/Data
COPY Indices /srv/shiny-server/igfstest/Indices
COPY VBGM_coeff /srv/shiny-server/igfstest
COPY IGFStest.Rproj /srv/shiny-server/igfstest/
COPY README.md /srv/shiny-server/igfstest/
COPY app.js /srv/shiny-server/igfstest/
COPY global.R /srv/shiny-server/igfstest/
COPY google-analytics.js /srv/shiny-server/igfstest/
COPY server.R /srv/shiny-server/igfstest/
COPY ui.R /srv/shiny-server/igfstest/

EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"]
