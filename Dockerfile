FROM fpco/stack-build:lts-13.17 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc

FROM ubuntu:16.04
RUN mkdir -p /opt/app/db
RUN mkdir -p /opt/app
ARG BINARY_PATH
WORKDIR /opt/app
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-13.17/8.6.4/bin .
CMD ["/opt/app/pi-monitor-exe"]
