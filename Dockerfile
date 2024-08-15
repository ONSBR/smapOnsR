FROM rocker/tidyverse:latest

# Copy the repo files into the Docker image
WORKDIR /build
COPY . .

# Install dependencies
RUN R -e "devtools::install(dependencies = TRUE)"
