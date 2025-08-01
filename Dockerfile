FROM rocker/r-ver:4.5.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install plumber with dependencies
RUN Rscript -e "install.packages(c('plumber', 'dplyr', 'readr', 'writexl', 'haven', 'readxl', 'arrow'))"

# Set working directory
WORKDIR /app

# Copy your API file into the image
COPY plumber.R /app/plumber.R

# Expose port
EXPOSE 8000

# Command to run your plumber API
# CMD ["Rscript", "-e", "pr <- plumber::plumb('/app/api.R'); pr$run(host='0.0.0.0', port=8000)"]
CMD ["Rscript", "/app/plumber.R"]