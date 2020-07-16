FROM node:13.7.0
COPY . /app
WORKDIR /app
RUN npm install
RUN npm run elm:spa:build
