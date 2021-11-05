let Main = require('./output/Main');

let main = () => {
  Main.application()
}

if (module.hot) {
  module.hot.accept(() => {
    console.log('Reloaded, running main again')
    main()
  })
}

console.log('Starting app')

main()
