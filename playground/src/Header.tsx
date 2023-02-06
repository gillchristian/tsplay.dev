import React from 'react'
import {css} from 'goober'

export const Header = () => (
  <header className={header}>
    <h2 className={h2}>
      <a
        href="https://tsplay.dev"
        target="_blank"
        rel="noopener noreferrer"
        className={tsLink}
      >
        tsplay.dev
      </a>
    </h2>

    <a
      href="https://www.buymeacoffee.com/gillchristian"
      target="_blank"
      rel="noopener noreferrer"
      className={burrito}
    >
      Support this plugging 🌯
    </a>
  </header>
)

const header = css`
  display: flex;
  align-items: center;
  flex-direction: column;

  margin-bottom: 1rem;
`

const h2 = css`
  margin: 0 0 0.5rem 0 !important;
  line-height: 1;
  text-align: center;
  color: #007acc;
`

const tsLink = css`
  color: #262626 !important;
  font-size: 1.3rem;
`

const burrito = css`
  color: #262626 !important;
  font-size: 0.8rem;
  font-weight: bold;
  text-decoration: none;
`
