const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = function config({ isDev = false } = {}) {
  return {
    mode: isDev ? 'development' : 'production',
    node: {
      fs: 'empty',
    },
    entry: {
      client: path.resolve(__dirname, 'src/client.js'),
      server: path.resolve(__dirname, 'src/server.js'),
    },
    output: {
      filename: isDev ? '[name].js' : '[name].[hash].js',
      path: path.resolve(__dirname, 'public'),
    },
    module: {
      rules: [
        {
          exclude: /node_modules|elm-stuff/,
          test: /\.js$/,
          use: {
            loader: 'babel-loader',
            options: {
              presets: ['@babel/preset-env'],
            },
          },
        },
        {
          exclude: /node_modules|elm-stuff/,
          test: /\.elm$/,
          use: 'elm-webpack-loader',
        },
      ],
    },
    plugins: [
      new HtmlWebpackPlugin({
        chunks: ['client'],
      }),
      new HtmlWebpackPlugin({
        chunks: ['server'],
        filename: 'server',
      }),
    ],
    devServer: {
      port: 8000,
    },
  };
};
