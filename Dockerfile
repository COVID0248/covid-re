FROM rocker/verse:4.0.3 AS dev
RUN eval "$(ssh-agent -s)"
RUN ssh-add ~/.ssh/id_rsa
RUN ssh -T git@github.com

# Build and run for development
# docker build --target dev -t covid-re:dev .
# sudo docker run \
#     --rm \
#     --name covid-re \
#     -d \
#     -p 8787:8787 \
#     -e "ROOT=TRUE" \
#     -e PASSWORD=123 \
#     -v $HOME/.gitconfig:/home/rstudio/.gitconfig \
#     -v $HOME/.ssh:/home/rstudio/.ssh \
#     covid-re:dev