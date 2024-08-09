FROM r-base:latest

# Install ssl, curl, and ssh
RUN apt-get update -qq && \
    apt-get install -y libssl-dev libcurl4-openssl-dev libssh2-1-dev && \
    apt-get install -y libharfbuzz-dev libfribidi-dev && \
    apt-get install -y libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev && \
    apt-get install -y libfontconfig1-dev libxml2-dev && \
    apt-get clean

# Copy the R script
WORKDIR /app
COPY . .

# Install dependencies
RUN Rscript packages.R
