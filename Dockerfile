FROM haskell:8
MAINTAINER Minyoung Jeong <kkungkkung@gmail.com>

WORKDIR /opt/nirum

# Add just the .cabal file to capture dependencies
COPY ./package.yaml /opt/nirum/package.yaml
COPY ./stack.yaml /opt/nirum/stack.yaml

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack build --only-snapshot

COPY . /opt/nirum
RUN stack build --copy-bins

ENV CMD=nirum
ENTRYPOINT ["./docker-entrypoint.sh"]
