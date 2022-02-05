FROM docker.uclv.cu/haskell:8.10.7

RUN mkdir app

WORKDIR /app

RUN stack init && stack install random
