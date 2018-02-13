# To correctly make a statically-linked binary, we use Alpine Linux.
# The distro entirely uses musl instead of glibc which is unfriendly to be
# statically linked.
FROM alpine:3.7 AS build

RUN apk add --no-cache \
        bash=4.4.19-r1 \
        build-base=0.5-r0 \
        curl=7.58.0-r0 \
        ghc=8.0.2-r6 \
        zlib-dev=1.2.11-r1
RUN curl -sSL https://get.haskellstack.org/ | bash

RUN stack config set system-ghc --global true

# Add just the package.yaml file to capture dependencies
COPY package.yaml /src/nirum/package.yaml
COPY stack.yaml /src/nirum/stack.yaml

WORKDIR /src/nirum

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack setup
RUN stack install --only-dependencies --test --no-run-tests
RUN stack build --only-snapshot --flag nirum:static

COPY . /src/nirum

RUN mkdir -p "/root/.local/bin"
RUN stack build --flag nirum:static --copy-bins

FROM alpine:3.7

RUN apk add --no-cache bash=4.4.19-r1

RUN mkdir -p /bin
COPY --from=build /root/.local/bin/nirum /bin/
COPY --from=build /src/nirum/docker-entrypoint.sh /bin/
RUN chmod +x /bin/nirum /bin/docker-entrypoint.sh

ENV CMD=/bin/nirum
ENTRYPOINT /bin/docker-entrypoint.sh
