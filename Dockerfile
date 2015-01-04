FROM haskell

RUN cabal update
RUN cabal install mtl
RUN cabal install network

ADD HIRC.hs /
RUN ghc /HIRC.hs

ENTRYPOINT /HIRC
