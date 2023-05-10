FROM deemp/breaking-news-back-dev:latest AS dev
RUN mkdir /tmp
COPY back /code/back
WORKDIR /code/back
RUN cabal build --enable-executable-static
RUN bash -c "cp $(cabal exec which back) /bin"

FROM dev
RUN rm -r /nix/store/*-doc
COPY --from=dev /bin/back /bin
CMD /bin/back