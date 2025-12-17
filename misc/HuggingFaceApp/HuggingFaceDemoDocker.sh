# syntax=docker/dockerfile:1
FROM rocker/r2u:22.04
WORKDIR /code

ARG DEBIAN_FRONTEND=noninteractive

# ------------------------------------------------------------------------------
# System deps (also needed if we have to compile any R package from source)
# ------------------------------------------------------------------------------
RUN apt-get update -y && apt-get install -y --no-install-recommends \
    wget bzip2 git unzip ca-certificates locales tzdata \
    build-essential gfortran \
    libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev \
    libopenblas-dev liblapack-dev \
    libjpeg-dev libpng-dev libtiff5-dev \
  && rm -rf /var/lib/apt/lists/*

ENV LC_ALL=C.UTF-8 LANG=C.UTF-8 HF_HUB_DISABLE_TELEMETRY=1 PIP_NO_CACHE_DIR=1

# ------------------------------------------------------------------------------
# Try to install as many R packages as possible via Debian (blazing fast)
# If any are missing from the repo, skip them and install later in R.
# NOTE: Do NOT include r-cran-parallel (parallel ships with base R).
# ------------------------------------------------------------------------------
ARG APT_R_PKGS="\
  r-cran-shiny r-cran-dplyr r-cran-ggplot2 r-cran-readr r-cran-ggextra \
  r-cran-dt r-cran-shinydashboard r-cran-reticulate \
  r-cran-remotes r-cran-data.table r-cran-stringdist r-cran-fastmatch \
  r-cran-stringr r-cran-rfast r-cran-foreach r-cran-doparallel r-cran-digest \
  r-cran-text"
RUN set -eux; \
    apt-get update -y; \
    for pkg in $APT_R_PKGS; do \
      if apt-cache show "$pkg" >/dev/null 2>&1; then \
        echo "Installing $pkg via apt ..."; \
        if ! apt-get install -y --no-install-recommends "$pkg"; then \
          echo "WARN: apt install failed for $pkg; will try R fallback."; \
        fi; \
      else \
        echo "INFO: $pkg not found in apt; will try R fallback."; \
      fi; \
    done; \
    rm -rf /var/lib/apt/lists/*

# ------------------------------------------------------------------------------
# R fallback: install anything still missing (pak first for speed, then base R)
# 'parallel' is part of base R; not needed here.
# 'text' is optional; failure is non-fatal.
# ------------------------------------------------------------------------------
# R fallback: install anything still missing (pak first, then base R)
RUN Rscript - <<'RSCRIPT'
options(Ncpus = parallel::detectCores())

cran <- "https://cloud.r-project.org"

# Required and optional packages
req <- c(
  "shiny","dplyr","ggplot2","readr","ggExtra","DT","shinydashboard",
  "reticulate","remotes","data.table","stringdist","fastmatch",
  "stringr","Rfast","foreach","doParallel","digest",

  # additional packages for shiny app
 "bslib", "shinyWidgets", "bsplus","magrittr"
)
opt <- c("text")  # non-fatal if unavailable

installed <- rownames(installed.packages())

# Install required set: prefer pak (fast), fall back to install.packages
need <- setdiff(req, installed)
if (length(need)) {
  if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak", repos = "https://r-lib.github.io/p/pak/stable")
  }
  ok <- tryCatch({
    pak::pak(need)
    TRUE
  }, error = function(e) FALSE)

  if (!ok) install.packages(need, repos = cran)
}

# Optional packages: best-effort only
opt_need <- setdiff(opt, rownames(installed.packages()))
if (length(opt_need)) {
  try(install.packages(opt_need, repos = cran), silent = TRUE)
}
RSCRIPT

# ------------------------------------------------------------------------------
# Install LinkOrgs from GitHub (retry once on transient errors)
# ------------------------------------------------------------------------------
RUN R -q -e "remotes::install_github('cjerzak/LinkOrgs-software/LinkOrgs', upgrade='never')" \
 || R -q -e "remotes::install_github('cjerzak/LinkOrgs-software/LinkOrgs', upgrade='never', dependencies=TRUE)"

# ------------------------------------------------------------------------------
# Miniforge (includes mamba by default) for Python ML backend
# ------------------------------------------------------------------------------
RUN wget -q https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-x86_64.sh -O /tmp/m.sh \
 && bash /tmp/m.sh -b -p /opt/conda \
 && rm /tmp/m.sh \
 && /opt/conda/bin/conda clean -afy

# After installing Miniforge, add proper conda initialization
RUN /opt/conda/bin/conda init bash && \
    echo "conda activate base" >> ~/.bashrc

# Set more comprehensive conda environment variables
ENV CONDA_DEFAULT_ENV=LinkOrgs_env \
    CONDA_PREFIX=/opt/conda/envs/LinkOrgs_env \
    CONDA_PYTHON_EXE=/opt/conda/bin/python \
    CONDA_EXE=/opt/conda/bin/conda \
    PATH=/opt/conda/envs/LinkOrgs_env/bin:/opt/conda/bin:$PATH

# After creating the environment, ensure it's properly registered
RUN mamba create -y -n LinkOrgs_env python=3.11 pip && \
    /opt/conda/envs/LinkOrgs_env/bin/pip install --only-binary=:all: \
        tensorflow==2.15 \
        numpy==1.26.4 \
        tensorflow_probability==0.23 \
        jax==0.4.26 \
        jaxlib==0.4.26 \
        optax==0.2.2 \
        equinox==0.11.4 \
        jmp==0.0.4 \
    || (echo 'Pip wheels missing; falling back to conda-forge where possible...' && \
        mamba install -y -n LinkOrgs_env -c conda-forge \
            tensorflow=2.15 numpy=1.26.4 tensorflow-probability=0.23 \
            jax=0.4.26 jaxlib=0.4.26 optax=0.2.2 equinox=0.11.4 && \
        /opt/conda/envs/LinkOrgs_env/bin/pip install jmp==0.0.4 && \
        /opt/conda/bin/conda clean -afy) && \
    # Important: Create a conda env config file for reticulate
    echo "/opt/conda/envs/LinkOrgs_env/bin/python" > /opt/conda/envs/LinkOrgs_env/.conda_prefix

# Keep RETICULATE_PYTHON but also add RETICULATE_CONDA
ENV RETICULATE_PYTHON=/opt/conda/envs/LinkOrgs_env/bin/python \
    RETICULATE_CONDA=/opt/conda/bin/conda

# ------------------------------------------------------------------------------
# App code
# ------------------------------------------------------------------------------
COPY . .

# ------------------------------------------------------------------------------
# Shiny entrypoint (honor HF $PORT; default 7860)
# ------------------------------------------------------------------------------
EXPOSE 7860
CMD ["R", "--quiet", "-e", "port <- as.integer(Sys.getenv('PORT', '7860')); shiny::runApp('/code', host='0.0.0.0', port=port)"]


