FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny', 'readxl', 'dplyr', 'tidyr', 'Rtsne', 'umap', 'ggplot2', 'plotly', 'conflicted'), repos='https://cran.rstudio.com/')"

WORKDIR /srv/shiny-server

COPY app.R .
COPY dnd_spells.xlsx .

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
