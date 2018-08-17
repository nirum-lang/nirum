# To correctly make a statically-linked binary, we use Alpine Linux.
# The distro entirely uses musl instead of glibc which is unfriendly to be
# statically linked.
FROM bitnami/minideb:jessie AS build

ARG APT_REPOSITORY

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN if [ "${APT_REPOSITORY}" != "" ]; then \
        sed -i 's|http://httpredir\.debian\.org/debian|'"${APT_REPOSITORY}|" \
        /etc/apt/sources.list; \
    fi

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
            build-essential ca-certificates curl \
            netbase \
    && \
    rm -rf /var/lib/apt/lists/*
RUN apt-get update && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    rm -rf /var/lib/apt/lists/*

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

FROM bitnami/minideb:jessie

RUN install_packages bash ca-certificates

RUN mkdir -p /bin
COPY --from=build /root/.local/bin/nirum /bin/
COPY --from=build /src/nirum/docker-entrypoint.sh /bin/
RUN chmod +x /bin/nirum /bin/docker-entrypoint.sh

ENV CMD=/bin/nirum
ENTRYPOINT ["/bin/docker-entrypoint.sh"]
