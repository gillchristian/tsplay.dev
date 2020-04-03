import React from 'react'

import './index.css'

import {App} from './App'
import {TsLogo} from './Logo'

export const Layout = () => (
  <div className="Site">
    <nav className="Nav">
      <TsLogo />
      <span>TypeScript</span>
    </nav>

    <div className="Main">
      <App />
    </div>

    <footer className="Footer">
      <div>
        <a
          href="https://gillchristian.xyz"
          target="_blank"
          rel="noopener noreferrer"
        >
          gillchristian
        </a>
        <span> / </span>
        <a
          href="https://github.com/gillchristian/tsplay.dev"
          target="_blank"
          rel="noopener noreferrer"
        >
          {'tsplay.dev'}
        </a>
      </div>
    </footer>
  </div>
)
