import Elm from './Main.elm'
import './main.css'

function parseIntOr(input, fallback) {
  const int = parseInt(input, 10)
  if (isNaN(int)) {
    return fallback
  }
  return int
}

const query = location.search.slice(1).split('&').reduce((pairs, s) => {
  const [key, value] = s.split('=')
  pairs[key] = value
  return pairs
}, {})

const currentDocumentIndex = parseIntOr(location.pathname.split('/').slice(-1), 0) - 1

const mountNode = document.getElementById('app')
const app = Elm.Main.embed(mountNode, {
  editModeInt: parseIntOr(query['editMode'], null),
  currentDocumentIndex
})
