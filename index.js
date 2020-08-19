const Main = require('./output/Main')

function main () {
  Main.main()
}

// HMR setup. For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function () {
    console.log('Reloaded, running main again')
    main()
  })
}

main()