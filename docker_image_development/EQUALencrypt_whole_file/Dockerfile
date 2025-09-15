# Use an official R runtime as a parent image
FROM r-base:latest 

# openssl does not install directly
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/* \
    && R -q -e 'install.packages("openssl")'

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinybusy', 'shinyjs', 'EQUALencrypt'))" 

# Set new workdir
WORKDIR /home/app

# Copy the app files into the Docker image
COPY src/ .

# Expose port
EXPOSE 3838

# Run the app
CMD ["R", "-e", "shiny::runApp('/home/app', host='0.0.0.0', port=3838)"]
