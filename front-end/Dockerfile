FROM node:10
RUN npm -g config set user root
RUN npm install -g create-elm-app
COPY ./ /app/
WORKDIR /app
CMD elm-app test
