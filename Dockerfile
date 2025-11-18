FROM docker.io/ocaml/opam:debian-ocaml-4.08 AS build
WORKDIR /src
ENV ELKHOUND_BUILD_DIR=/src/obj/elkhound
ENV ELKHOUND_SRC_DIR=/src/elkhound-master/src
ENV PATH="${PATH}:/home/opam/.opam/4.08/bin"
USER root
RUN apt-get update -yqqq && \
    apt-get install -yqqq bison cmake flex wget && \
    wget -q https://github.com/The-Mod-Elephant/elkhound/archive/refs/heads/master.zip && \
    unzip master.zip && \
    mkdir -p ${ELKHOUND_BUILD_DIR} && \
    cmake -Wno-dev -S ${ELKHOUND_SRC_DIR} -B ${ELKHOUND_BUILD_DIR} -D CMAKE_BUILD_TYPE=Release && \
    make -j$(npoc) -C ${ELKHOUND_BUILD_DIR} && \
    mv ${ELKHOUND_BUILD_DIR}/elkhound/elkhound /usr/bin/elkhound
# Weidu
FROM docker.io/ocaml/opam:debian-ocaml-4.08
COPY --from=build /usr/bin/elkhound /usr/bin/elkhound
WORKDIR /src
COPY . .
USER root
RUN chown -R opam:opam /src
USER opam
ENV PATH="${PATH}:/home/opam/.opam/4.08/bin"
RUN mv sample.Configuration Configuration && \
    make
CMD ["/src/weidu.asm.exe"]
