import('./Main.elm').then(Elm => {
  var mountNode = document.getElementById('app')
  var app = Elm.Main.embed(mountNode)
})