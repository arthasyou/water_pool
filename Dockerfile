# Build stage 0
FROM erlang:alpine

RUN apk update && apk upgrade && \
    apk add --no-cache bash git openssh
# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang application
COPY water water

# And build the release
WORKDIR /buildroot/water

RUN rebar3 as prod release

# Build stage 1
FROM alpine

# Install some libs
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs

# Install the released application
COPY --from=0 /buildroot/water/_build/prod/rel/water /water

# Expose relevant ports
EXPOSE 8080
# EXPOSE 8443

CMD ["/water/bin/water", "foreground"]
