FROM archlinux:base-devel

RUN pacman --noconfirm -Syyu && pacman --noconfirm -S zsh r pandoc noto-fonts && \
    mkdir -p /opt/ods-benchmarking

WORKDIR /opt/ods-benchmarking

COPY . .

ENV CRAN_URI=https://cloud.r-project.org

ENV R_PKG="c( \
  'rlang', 'purrr', 'clock', 'bit64', 'data.table', 'knitr', \
  'svglite', 'ggplot2', 'rmarkdown', 'huxtable' \
)"

RUN R -e "install.packages( \
  $R_PKG, repos = c(CRAN = '$CRAN_URI'), \
  INSTALL_opts = c('--no-docs', '--no-help', '--no-html') \
)"

ENTRYPOINT ./bin/run.sh
