# ------------------------------------------------------------------------------
# Use the R base image
# ------------------------------------------------------------------------------
FROM rocker/r-base:latest
# ------------------------------------------------------------------------------
# Switch to /code as our working directory
# ------------------------------------------------------------------------------
WORKDIR /code
# ------------------------------------------------------------------------------
# 1) Install system dependencies + Miniconda
# ------------------------------------------------------------------------------
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
        wget \
        bzip2 \
    && rm -rf /var/lib/apt/lists/*
# Install Miniconda to /opt/conda
RUN wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O /tmp/miniconda.sh \
    && /bin/bash /tmp/miniconda.sh -b -p /opt/conda \
    && rm /tmp/miniconda.sh \
    && /opt/conda/bin/conda clean -afy
# Make sure conda is on PATH
ENV PATH=/opt/conda/bin:$PATH
# ------------------------------------------------------------------------------
# 2) Install required R packages
# (We add reticulate + a few extras such as DT and shinydashboard
# since your app uses them.)
# ------------------------------------------------------------------------------
RUN install2.r --error \
    shiny \
    dplyr \
    ggplot2 \
    readr \
    ggExtra \
    DT \
    parallel \
    shinydashboard \
    reticulate \
    remotes
# ------------------------------------------------------------------------------
# 3) Copy your local code (including app.R and CODEBASE) into the container
# ------------------------------------------------------------------------------
COPY . .
# Install the LinkOrgs package from GitHub
RUN Rscript -e "remotes::install_github('cjerzak/LinkOrgs-software/LinkOrgs')"
# ------------------------------------------------------------------------------
# 4) (Optional) Pre-build the conda environment inside the Docker image
# by calling your 'BuildBackend()' function.
# This ensures that JAX/numpy are already installed.
# ------------------------------------------------------------------------------
RUN Rscript -e "library(LinkOrgs); LinkOrgs::BuildBackend(conda='auto')"
# ------------------------------------------------------------------------------
# 5) Expose the Shiny port and set default command to run the Shiny app
# ------------------------------------------------------------------------------
EXPOSE 7860
CMD ["R", "--quiet", "-e", "shiny::runApp('/code', host='0.0.0.0', port=7860)"]












# OLD

FROM rocker/r-base:latest

# Install Miniconda for Python/Conda support (required for ML backend)
RUN apt-get update && apt-get install -y wget && \
    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O /tmp/miniconda.sh && \
    bash /tmp/miniconda.sh -b -p /opt/conda && \
    rm /tmp/miniconda.sh && \
    /opt/conda/bin/conda clean --all && \
    ln -s /opt/conda/etc/profile.d/conda.sh /etc/profile.d/conda.sh && \
    echo ". /opt/conda/etc/profile.d/conda.sh" >> ~/.bashrc && \
    echo "conda activate base" >> ~/.bashrc && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

ENV PATH="/opt/conda/bin:${PATH}"

# Set the working directory inside the container
WORKDIR /code

# Install additional R packages required by the app and LinkOrgs
RUN Rscript -e "install.packages(c('devtools', 'reticulate', 'shiny', 'shinydashboard', 'leaflet', 'DT', 'RColorBrewer', 'plyr', 'dplyr', 'data.table', 'fastmatch', 'stringdist', 'stringr', 'Rfast', 'doParallel'), dependencies = c('Depends', 'Imports', 'LinkingTo'), repos = 'https://cran.r-project.org')"

# Install LinkOrgs from GitHub
RUN Rscript -e "devtools::install_github('cjerzak/LinkOrgs-software/LinkOrgs')"

# Build the LinkOrgs ML backend (downloads models from Hugging Face and sets up Conda env)
RUN Rscript -e "LinkOrgs::BuildBackend(conda_env = 'LinkOrgsEnv', conda = 'auto')"

# Copy the current directory (containing app.R and any demo data/files) into the container
COPY . .

# Expose port 7860 for Hugging Face Spaces
EXPOSE 7860

# Run the Shiny app, making it accessible externally on port 7860
CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
