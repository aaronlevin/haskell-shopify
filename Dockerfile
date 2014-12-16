FROM haskell:7.8

RUN cabal update

ADD ./shopify.cabal /opt/library/shopify.cabal
RUN cd /opt/library && cabal install --enable-tests --only-dependencies -j4

ADD . /opt/library
RUN cd /opt/library && cabal install --enable-tests

