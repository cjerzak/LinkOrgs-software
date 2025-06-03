#!/usr/bin/env bash
set -euo pipefail

#
# 0. (Optional) If you already ran something that left a half‐configured conda,
#    you might want to remove it first. For example:
#    rm -rf /opt/conda
#
#    (Skip this if you know /opt/conda doesn't exist.)
#

#------------------------------------------------------------------------------
# 1. Install system-level R (and OS dependencies)
#------------------------------------------------------------------------------
apt-get update \
  && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
       r-base \
       r-base-dev \
       libcurl4-openssl-dev \
       libssl-dev \
       libxml2-dev \
       wget \
       bzip2 \
       build-essential \
       git \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# Set up MAKEFLAGS so that any compilation inside R (from source) uses all cores.
# This helps speed up any package builds that still must compile.
export MAKEFLAGS="-j$(nproc)"

#------------------------------------------------------------------------------
# 2. Install Miniconda into /opt/conda
#------------------------------------------------------------------------------
readonly MINICONDA_SH="/tmp/Miniconda3-latest-Linux-x86_64.sh"
wget --quiet "https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh" -O "$MINICONDA_SH"
bash "$MINICONDA_SH" -b -p /opt/conda
rm "$MINICONDA_SH"

# Make sure conda is on PATH
export PATH="/opt/conda/bin:${PATH}"

# Initialize conda for this shell
if [[ -f "/opt/conda/etc/profile.d/conda.sh" ]]; then
  source "/opt/conda/etc/profile.d/conda.sh"
else
  echo "Error: /opt/conda/etc/profile.d/conda.sh not found" >&2
  exit 1
fi



#------------------------------------------------------------------------------
# 4. Install required R packages
#    – First, install Debian binaries for common CRAN packages
#      (much faster than building from source).
#    – Then fall back to install.packages() for anything still missing.
#------------------------------------------------------------------------------
# 4.a. Install Debian‐packaged R libraries if available
apt-get update \
  && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
  r-cran-reticulate \
  r-cran-remotes \
  r-cran-rcpp          r-cran-rlang         r-cran-lifecycle   r-cran-glue \
  r-cran-cli           r-cran-withr         r-cran-vctrs       r-cran-utf8 \
  r-cran-pkgconfig     r-cran-pillar        r-cran-magrittr    r-cran-fansi \
  r-cran-iterators     r-cran-foreach       r-cran-jsonlite    r-cran-rcpptoml \
  r-cran-tidyselect    r-cran-tibble        r-cran-r6          r-cran-generics \
  r-cran-stringi       r-cran-rcppeigen     r-cran-shape       r-cran-rcpparmadillo \
  r-cran-rcppparallel  r-cran-fastmatch     r-cran-dplyr       r-cran-stringr \
  r-cran-glmnet        r-cran-data.table    r-cran-reticulate  r-cran-remotes \
  r-cran-r.methodss3   r-cran-r.utils       r-cran-r.oo        r-cran-fansi \
  r-cran-plyr  r-cran-stringdist \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# 4.b. For any packages not covered by Debian binaries, use Rscript in parallel mode:
#      – options(Ncpus=detectCores()) speeds up multi‐package installs
Rscript -e "
  options(
    repos = c(CRAN = 'https://cloud.r-project.org'),
    install.packages.compile.from.source = 'never'  # prefer binaries when available
  );
"

# Quickly install other binaries 
BIN_ROOT="https://github.com/cjerzak/LinkOrgs-software/raw/refs/heads/master/misc/DockerSetup/binaries/"

# Tarballs you produced in the builder container
for f in Rfast_2.1.5.1_R_x86_64-pc-linux-gnu.tar.gz
do
  curl -sSL -o "/tmp/$f" "$BIN_ROOT/$f"
done

# First library path where R looks for user packages
LIB="$(Rscript -e 'cat(.libPaths()[1])')"
mkdir -p "$LIB"

# Untar each package directly into the library path (no R CMD INSTALL)
for f in /tmp/*.tar.gz; do
  tar -xzf "$f" -C "$LIB"
done
rm /tmp/*.tar.gz

#------------------------------------------------------------------------------
# 5. Install your GitHub-hosted "LinkOrgs" package
#    We still need remotes::install_github, but we pass options(Ncpus) and turn
#    off vignette building so it doesn’t crawl CRAN for hundreds of MB.
#------------------------------------------------------------------------------
Rscript -e "
  install.packages('zigg');
   remotes::install_github(
    'cjerzak/LinkOrgs-software',
    subdir = 'LinkOrgs',
    dependencies = FALSE,
    build_vignettes = FALSE
  )
"

#------------------------------------------------------------------------------
# 6. Pre-build the conda-backed JAX/numpy environment by calling BuildBackend()
#------------------------------------------------------------------------------
Rscript -e "library(LinkOrgs); LinkOrgs::BuildBackend(conda_env = 'LinkOrgsEnv', conda = 'auto')"

echo
echo "======================================"
echo " Setup complete."
echo " • R packages installed (via Debian binaries + parallel builds)."
echo " • Conda env 'LinkOrgsEnv' with binutils, JAX, numpy, etc., is ready."
echo " • To enter the environment, run 'conda activate LinkOrgsEnv'"
echo "======================================"
