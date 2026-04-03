FROM rocker/r-ver:4.5.2

ENV DEBIAN_FRONTEND=noninteractive
ENV WHICHISBETTER_DATA_DIR=/var/lib/whichisbetter

RUN apt-get update && apt-get install -y --no-install-recommends \
    python3 \
    python3-pip \
    build-essential \
    gfortran \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY scripts/install_hosted_deps.R /tmp/install_hosted_deps.R
RUN Rscript /tmp/install_hosted_deps.R

COPY . /app

RUN mkdir -p "${WHICHISBETTER_DATA_DIR}"

EXPOSE 10000

CMD ["Rscript", "scripts/run_hosted.R"]
