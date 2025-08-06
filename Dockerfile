FROM ubuntu:22.04

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive \
    PATH="/root/.cabal/bin:/root/.ghcup/bin:${PATH}" \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    C_INCLUDE_PATH=/usr/local/include \
    LIBRARY_PATH=/usr/local/lib \
    LD_LIBRARY_PATH=/usr/local/lib

# Install system dependencies
RUN apt-get update && \
    apt-get install -y \
      curl \
      build-essential \
      libtool \
      m4 \
      automake \
      autoconf \
      git \
      sudo \
      libgmp-dev \
      zlib1g-dev \
      libssl-dev \
      pkg-config \
      python3 \
      python3-pip \
      flex \
      bison \
      libc6-dev \
      gnupg \
      ca-certificates \
      libffi-dev \
      perl

# Install GHCup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Install GHC and Cabal
RUN ghcup install ghc 9.2.8 && \
    ghcup set ghc 9.2.8 && \
    ghcup install cabal 3.10.2.1 && \
    ghcup set cabal 3.10.2.1

# Verify versions
RUN ghc --version && cabal --version

# Install haskell tools
RUN cabal update && \
    cabal install alex happy BNFC

# Clone specific CUDD release and fix autotools timestamp issue
WORKDIR /
RUN git clone --branch cudd-3.0.0 https://github.com/ivmai/cudd && \
    cd cudd && \
    touch aclocal.m4 configure Makefile.in

WORKDIR /cudd

# Configure and build CUDD, fix missing config.h in mtr/
RUN ./configure --enable-obj --enable-dddmp --enable-shared && \
    make && \
    make check && \
    make install && \
    cp config.h /usr/local/include/ && \
    cp util/util.h /usr/local/include/ && \
    cp mtr/mtr.h /usr/local/include/

# Install CUDD Haskell bindings
RUN cabal install --lib cudd

# Copy project files
WORKDIR /app
COPY . .

# Build the project
RUN cabal update && \
    cabal build

# Default shell for running benchmarks or interacting
CMD ["/bin/bash"]
