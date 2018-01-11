import Elm from './Main.elm'
import './main.css'

var mountNode = document.getElementById('app')
var app = Elm.Main.embed(mountNode)
