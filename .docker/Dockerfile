FROM haskell

RUN apt-get update && apt-get install -y build-essential pkg-config libpcre3-dev
RUN cabal update

VOLUME /code
WORKDIR /code
