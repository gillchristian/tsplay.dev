import React from 'react'
import {css} from 'goober'

export const Header = () => (
  <header className={header}>
    <h2 className={h2}>Playground Link Shortener</h2>
    <a
      href="https://tsplay.dev"
      target="_blank"
      rel="noopener noreferrer"
      className={tsLink}
    >
      tsplay.dev
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
  margin-bottom: 1rem;
  font-size: 1.3rem;
`
