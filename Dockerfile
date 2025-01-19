# Base R image
FROM rocker/r-ver:latest

# Create a directory in the container
RUN mkdir /home/r-environment

# Install pak
RUN R -e "install.packages('pak', repos = 'https://r-lib.github.io/p/pak/stable/')"
RUN R -e "pak::pak_update()"

# Copy the local package directory to the container
COPY . /home/r-environment/crricket

# Install the local package using pak
RUN R -e "pak::local_install('/home/r-environment/crricket')"

# Copy your R script to the container
COPY R/main.R /home/r-environment/main.R

# Set the working directory
WORKDIR /home/r-environment

# Run the R script
CMD ["Rscript", "main.R"]
