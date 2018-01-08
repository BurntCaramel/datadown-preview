console.log('load')

import('./main.elm').then(Elm => {
  var mountNode = document.getElementById('app')
  var app = Elm.Main.embed(mountNode)
})