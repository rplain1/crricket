# Base image https://hub.docker.com/u/rocker/
FROM rocker/rstudio

# system libraries of general use
## install debian packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    build-essential



# Install pak and its dependencies correctly
RUN R -e 'install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))'
RUN R -e "pak::pak_update()"
RUN R -e "pak::pkg_install(c('arrow','assertthat','checkmate','DBI','dplyr','duckdb','glue','rlang','stringr'))"
RUN R -e "pak::pkg_install('rplain1/crricket')"

# Create a working directory
WORKDIR /home/r-environment

# Copy the local package to the container
COPY . /home/r-environment/crricket

# Copy the R script to the container
COPY R/main.R /home/r-environment/main.R

# Run the R script
CMD ["Rscript", "main.R"]
#CMD ["R"]
