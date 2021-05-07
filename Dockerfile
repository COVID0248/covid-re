FROM rocker/verse:4.0.3
RUN apt-get update
RUN apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev
RUN install2.r --error --deps TRUE rstan

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
