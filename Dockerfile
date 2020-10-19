FROM haskell:8.10.2-buster as builder

WORKDIR /opt/service

# Install our dependencies
RUN apt-get update -y \
&& apt-get upgrade -y \
&& apt-get install -y \
  curl \
  xz-utils \
  gcc \
  make \
  libtinfo5 \
  libgmp-dev \
  zlib1g-dev

# Move the configuration for the project to build our dependencies.
COPY ./haskell-server-template.cabal /opt/service/haskell-server-template.cabal

RUN cabal update \
&& cabal build --only-dependencies -j4

COPY . /opt/service
RUN cabal install --installdir=/usr/local/bin

FROM debian:buster

EXPOSE 8080
EXPOSE 8081

WORKDIR /opt/service

# Install our dependencies
RUN apt-get update -y \
&& apt-get upgrade -y \
&& apt-get install -y \
  curl \
  xz-utils \
  gcc \
  make \
  libtinfo5 \
  libgmp-dev \
  git \
  zlib1g-dev

COPY ./config.json /opt/service/config.json

COPY --from=builder /usr/local/bin/server /usr/local/bin/server

RUN git clone https://github.com/tibbe/ekg \
&& mv ekg/assets . \
&& rm -rf ekg \
&& mkdir ekg \
&& mv assets ekg

ENV ekg_datadir=/opt/service/ekg

CMD ["server", "run"]
