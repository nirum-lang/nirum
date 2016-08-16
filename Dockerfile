FROM haskell:latest
MAINTAINER Minyoung Jeong <kkungkkung@gmail.com>

WORKDIR /opt/nirum
COPY . /opt/nirum

RUN cabal update
RUN cabal sandbox init && \
    cabal install --only-dependencies && \
    cabal configure && \
    cabal build

ENTRYPOINT ["dist/build/nirum/nirum"]
