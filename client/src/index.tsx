import * as React from 'react'
import * as ReactDOM from 'react-dom'
import GlobalStyles from './styles/GlobalStyles'
import App from './App'
import * as registerServiceWorker from './utils/serviceWorker'
import GlobalToast from './components/GlobalToast'
import 'react-toastify/dist/ReactToastify.min.css'

ReactDOM.render(
  <React.Fragment>
    <GlobalStyles />
    <GlobalToast />
    <App />
  </React.Fragment>,
  document.getElementById('root')
)
registerServiceWorker.register()
