FROM docker.io/ocaml/opam:debian-13-ocaml-4.08 AS elkhound-build
WORKDIR /src
ENV ELKHOUND_BUILD_DIR=/src/obj/elkhound
ENV ELKHOUND_SRC_DIR=/src/elkhound-master/src
ENV PATH="${PATH}:/home/opam/.opam/4.08/bin"
USER root
RUN apt-get update -yqqq && \
    apt-get install -yqqq bison cmake flex wget && \
    wget -q https://github.com/WeiDUorg/elkhound/archive/refs/heads/master.zip && \
    unzip master.zip && \
    mkdir -p ${ELKHOUND_BUILD_DIR} && \
    cmake -Wno-dev -S ${ELKHOUND_SRC_DIR} -B ${ELKHOUND_BUILD_DIR} -D CMAKE_BUILD_TYPE=Release && \
    make -j$(nproc) -C ${ELKHOUND_BUILD_DIR} && \
    mv ${ELKHOUND_BUILD_DIR}/elkhound/elkhound /usr/bin/elkhound
FROM docker.io/ocaml/opam:debian-13-ocaml-4.08 AS weidu-build
COPY --from=elkhound-build /usr/bin/elkhound /usr/bin/elkhound
WORKDIR /src
COPY . .
USER root
RUN chown -R opam:opam /src
USER opam
ENV PATH="${PATH}:/home/opam/.opam/4.08/bin"
RUN make
FROM docker.io/debian:13-slim
WORKDIR /src
COPY --from=weidu-build /src/weidu.asm.exe /src/weidu
CMD ["/src/weidu"]
