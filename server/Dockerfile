# Reference:
# https://www.dev-log.me/Deploying_Haskell:_Painless_CICD_with_Travis,_Docker_and_Digital_Ocean_(or_any_linux_VM)/
FROM ubuntu:16.04

RUN mkdir -p /opt/tsplay/

ARG BINARY_PATH

WORKDIR /opt/tsplay

RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev \
  netbase

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

COPY "$BINARY_PATH" /opt/tsplay

EXPOSE 8080

CMD ["/opt/tsplay/tsplay-exe"]

