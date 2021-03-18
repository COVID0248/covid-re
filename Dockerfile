# Setup git in the container
# eval "$(ssh-agent -s)"
# ssh-add ~/.ssh/id_rsa
# ssh -T git@github.com

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
#     -v $HOME/.aws:/home/rstudio/.aws \
#     -v $HOME/.ssh:/home/rstudio/.ssh \
#     rocker/verse:4.0.3