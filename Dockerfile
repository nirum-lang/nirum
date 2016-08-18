FROM haskell:8
MAINTAINER Minyoung Jeong <kkungkkung@gmail.com>

WORKDIR /opt/nirum

# Add just the .cabal file to capture dependencies
COPY ./nirum.cabal /opt/nirum/nirum.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal update && cabal install --only-dependencies -j4

COPY . /opt/nirum
RUN cabal install

ENV CMD=nirum
ENTRYPOINT ["./docker-entrypoint.sh"]
