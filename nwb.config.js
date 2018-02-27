const tailwindcss = require('tailwindcss')

module.exports = {
  type: 'web-app',
  polyfill: false, // Not needed by Elm
  webpack: {
    rules: {
      postcss: {
        plugins: [
          tailwindcss('./src/tailwind.js'),
          require('autoprefixer')
        ]
      }
    },
    extra: {
      module: {
        rules: [{
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader:  'elm-webpack-loader?verbose=true&warn=true&pathToMake=./node_modules/.bin/elm-make',
        }]
      }
    },
    autoprefixer: {
      "browsers": [
        ">1%",
        "last 4 versions",
        "Firefox ESR",
        "not ie < 9"
      ],
      "flexbox": "no-2009"
    }
  },
  devServer: {
    port: 8800
  }
}
