FROM rocker/verse:4.0.4

RUN apt-get update
RUN apt-get install -y --no-install-recommends apt-utils ed libnlopt-dev
RUN apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev
RUN apt-get clean
RUN rm -rf /var/lib/apt/lists/

# Install rstan
RUN install2.r --error --deps TRUE rstan
RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Global site-wide config -- neeeded for building packages
RUN mkdir -p $HOME/.R/
RUN echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -flto -ffat-lto-objects  -Wno-unused-local-typedefs \n" >> $HOME/.R/Makevars

# Config for rstudio user
RUN mkdir -p $HOME/.R/
RUN echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -flto -ffat-lto-objects  -Wno-unused-local-typedefs -Wno-ignored-attributes -Wno-deprecated-declarations\n" >> $HOME/.R/Makevars
RUN echo "options(mc.cores = parallel::detectCores())\n" >> /home/rstudio/.Rprofile
RUN echo "rstan::rstan_options(auto_write = TRUE)\n" >> /home/rstudio/.Rprofile

# # Build and run for development
# docker build --target dev -t covid-re .
# sudo docker run \
#     --rm \
#     -d \
#     -p 8787:8787 \
#     -e "ROOT=TRUE" \
#     -e PASSWORD=123 \
#     -v $HOME/.gitconfig:/home/rstudio/.gitconfig \
#     -v $HOME/.ssh:/home/rstudio/.ssh \
#     covid-re