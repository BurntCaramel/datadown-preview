import Elm from './Main.elm'
import './main.css'

const mountNode = document.getElementById('app')
const app = Elm.Main.embed(mountNode, {
})
