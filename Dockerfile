FROM haskell:latest
MAINTAINER Minyoung Jeong <kkungkkung@gmail.com>

WORKDIR /opt/nirum
COPY . /opt/nirum

RUN cabal update
RUN cabal sandbox init && \
    cabal install --only-dependencies && \
    cabal configure && \
    cabal build

ENV CMD=dist/build/nirum/nirum
ENTRYPOINT ["./docker-entrypoint.sh"]
