# Docker Build (place x86 tarballs to binaries folder in GitHub)
# remove exisintg tarballs to avoid errors 
rm -rf ./binaries/*.gz

docker run --platform=linux/amd64 --rm \
  -v "$(pwd)/binaries:/binaries" \
  rocker/r-ver:4.4.0 bash -exc "
    set -euo pipefail

    ## 1) Install system development libraries (no R packages here)
    echo '🔧 Installing system development libraries…'
    apt-get update -qq
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      build-essential \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev

 # Add CRAN’s GPG key
 RUN apt-get update \
    && apt-get install -y --no-install-recommends dirmngr gnupg apt-transport-https ca-certificates \
    && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 51716619E084DAB9 \
    && echo \"deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/\" \
          > /etc/apt/sources.list.d/cran40.list \
     && apt-get clean \
     && rm -rf /var/lib/apt/lists/*

    apt-get update \
      && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      r-cran-rcpp           r-cran-rcppparallel   r-cran-rcpparmadillo \
      && apt-get clean \
      && rm -rf /var/lib/apt/lists/*

    ## 2) Make sure R can install any other pure‐R deps from CRAN
    #echo '📦 Installing pscl from CRAN…'
    Rscript -e 'install.packages(
      \"zigg\",
      repos = \"https://cloud.r-project.org\",
      dependencies = TRUE
    )'

    ## 3) Switch into /binaries so download.packages writes there
    cd /binaries

    ################################################################################
    ## 4) Build Rfast → “Rfast_<ver>_R_x86_64-pc-linux-gnu.tar.gz”
    echo '📦 Downloading Rfast source…'
    Rscript -e 'download.packages(
      \"Rfast\",
      destdir      = \"/binaries\",
      type         = \"source\",
      repos        = \"https://cloud.r-project.org\"
    )'
    echo '⚙️  Building Rcpp binary…'
    R CMD INSTALL --build /binaries/Rfast_*.tar.gz

    ################################################################################
    ## List out exactly what landed in /binaries
    echo '✅ Built binary tarballs:'
    ls -l /binaries
  "
