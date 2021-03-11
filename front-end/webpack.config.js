const path = require("path");

module.exports = {
  mode: "production",
  optimization: {
    usedExports: true,
  },
  entry: {
    main: "./public/main.js",
    nglComponent: "./public/ngl-web-component",
  },
  output: {
    filename: "[name].bundle.js",
    path: path.resolve(__dirname, "public/dist"),
  },
};
