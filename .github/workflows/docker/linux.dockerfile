################################################################################
# Dockerfile used for building linux/alpine binary
# taken from https://github.com/anmonteiro/gh-feed-reader/blob/master/Dockerfile
################################################################################

FROM node:12-alpine as build

ENV TERM=dumb LD_LIBRARY_PATH=/usr/local/lib:/usr/lib:/lib

WORKDIR /

RUN npm install --global --unsafe-perm esy@0.6.2

RUN apk add --no-cache ca-certificates wget bash curl perl-utils git patch \
  gcc g++ musl-dev make m4 linux-headers coreutils python

RUN wget -q -O /etc/apk/keys/sgerrand.rsa.pub https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub
RUN wget https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.28-r0/glibc-2.28-r0.apk
RUN apk add --no-cache glibc-2.28-r0.apk

ENTRYPOINT ["/bin/bash", "-c"]
