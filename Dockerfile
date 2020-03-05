FROM rocker/r-ver:3.6.2
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libssh2-1-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("cli",upgrade="never", version = "2.0.2")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("htmlwidgets",upgrade="never", version = "1.5.1")'
RUN Rscript -e 'remotes::install_version("inndxtdr",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "1.5")'
RUN Rscript -e 'remotes::install_version("markdown",upgrade="never", version = "1.1")'
RUN Rscript -e 'remotes::install_version("pins",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.3")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.1")'
RUN Rscript -e 'remotes::install_version("waiter",upgrade="never", version = "0.1.0")'
RUN Rscript -e 'remotes::install_version("yaml",upgrade="never", version = "2.2.1")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.28")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_github("JohnCoene/countup@b1469377fb663f8ec4320223870b826c7cb51d0c")'
RUN Rscript -e 'remotes::install_github("tidyverse/dplyr@db187556fc8cebe8c1edc2bd7865763bc47993ae")'
RUN Rscript -e 'remotes::install_github("JohnCoene/echarts4r@8c010c7b19877bb7749ae5160905df9d5a4153b8")'
RUN Rscript -e 'remotes::install_github("Thinkr-open/golem@893d311a16aab120cd33422e7d2290adf5d85558")'
RUN Rscript -e 'remotes::install_github("rstudio/htmltools@ce7605520dc6a80093ed079f78462cdc33f318b7")'
RUN Rscript -e 'remotes::install_github("rstudio/pool@0b03680bffafb3f79c33d8a21e0202aaf4aa3a8f")'
RUN Rscript -e 'remotes::install_github("JohnCoene/sever@3a42eba0d5a54b467dcc87a3abcfe25b84544d3f")'
RUN Rscript -e 'remotes::install_github("rstudio/shiny@fe9cc6038ec9fafbe3227df0450d0634e681a408")'
RUN Rscript -e 'remotes::install_github("daattali/shinyjs@52542599fe72957e66957ed04d20a719bc9cadd3")'
RUN Rscript -e 'remotes::install_github("RinteRface/shinyMobile@1dcb2320febe8ebaebe92dfa0aa700d0200c84a4")'
RUN Rscript -e 'remotes::install_github("JohnCoene/shinyscroll@60747ac7eedea960199113710d1797375f3417af")'
RUN Rscript -e 'remotes::install_github("tidyverse/tidyr@afa5e3a4b32b422641e5a4162a03d28a03341f8f")'
RUN Rscript -e 'remotes::install_github("r-lib/tidyselect@bb145af7fd700f7e5d24d206f5b5d70ef56b7b84")'
RUN Rscript -e 'remotes::install_github("r-lib/rlang@6592336a01dde4dc186d08d2483fb6b2ba58789b")'
RUN Rscript -e 'remotes::install_github("r-lib/pillar@8f5918cc528b427894513b0eeaf1abfbbfd9e5c2")'
RUN Rscript -e 'remotes::install_github("r-lib/vctrs@930c4977fd98d26425917bc024fe2c03297cb6b7")'
RUN Rscript -e 'remotes::install_github("tidyverse/tibble@4a7f01f3954afd9f146ef77cfb2a061e9d2ed7ad")'
RUN Rscript -e 'remotes::install_github("r-lib/here@600002a1bf74834adf67a72b348f24b0cf61682d")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');inndxweb::run_app()"
