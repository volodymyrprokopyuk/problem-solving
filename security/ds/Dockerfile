FROM archlinux:base-devel

RUN pacman --noconfirm -Syyu && \
    pacman --noconfirm -S libsodium r rhonabwy && \
    mkdir -p /opt/ds

ENV CRAN_URI=https://cloud.r-project.org
ENV R_PKG="c('yaml', 'plumber', 'httr', 'processx')"

RUN R -e "install.packages( \
    $R_PKG, repos = c(CRAN = '$CRAN_URI'), \
    INSTALL_opts = c('--no-docs', '--no-help', '--no-html') \
)"

WORKDIR /opt/ds

COPY . .

ENTRYPOINT ["Rscript", "server.R"]
CMD ["ds_config.yaml"]
