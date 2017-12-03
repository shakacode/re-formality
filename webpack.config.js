const path = require('path');

module.exports = {
  entry: {
    app: './examples/index.bs.js',
  },
  output: {
    path: path.join(__dirname, "examples", "build"),
    filename: '[name].js',
  },
};
