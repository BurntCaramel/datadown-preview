import Elm from './Main.elm'
import './main.css'

const query = location.search.slice(1).split('&').reduce((pairs, s) => {
  const [key, value] = s.split('=')
  pairs[key] = value
  return pairs
}, {})

function parseIntOrNull(input) {
  const int = parseInt(input, 10)
  if (isNaN(int)) {
    return null
  }
  return int
}

const mountNode = document.getElementById('app')
const app = Elm.Main.embed(mountNode, {
  editModeInt: parseIntOrNull(query['editMode'])
})
