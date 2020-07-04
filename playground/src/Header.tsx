import React, {useState} from 'react'
import {css} from 'goober'

const KEY = 'tsplay.dev:warning_dismissed'

export const Header = () => {
  const [showMsg, setShowMsg] = useState<boolean>(
    () => localStorage.getItem(KEY) !== 'true',
  )

  const dismissMsg = () => {
    localStorage.setItem(KEY, 'true')
    setShowMsg(false)
  }

  return (
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
      {showMsg && (
        <div className={warningClass}>
          <p className="warning-title">
            <b>
              <span role="img" aria-label="warning">
                ⚠️
              </span>{' '}
              This plugin is still a PoC (proof of concept){' '}
              <span role="img" aria-label="warning">
                ⚠️
              </span>
            </b>
          </p>
          <p className={p}>
            <a
              href="https://github.com/gillchristian/tsplay.dev"
              target="_blank"
              rel="noopener noreferrer"
            >
              github.com/gillchristian/tsplay.dev
            </a>
          </p>
          <p className={p}>
            <a
              href="https://discordapp.com/channels/508357248330760243/681155785673146479"
              target="_blank"
              rel="noopener noreferrer"
            >
              Discord #playground-plugins
            </a>
          </p>
          <button className={`${dismissClass} button`} onClick={dismissMsg}>
            Dismiss
          </button>
        </div>
      )}
    </header>
  )
}

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

const p = css`
  line-height: 1;
  margin-bottom: 0.25rem;
  margin-top: 0.25rem;
`

const tsLink = css`
  color: #262626 !important;
  margin-bottom: 1rem;
  font-size: 1.3rem;
`

const warningClass = css`
  color: #262626;
  background-color: #fff;
  border: 1px solid #262626;
  padding: 0.5rem 1rem;
  margin-bottom: 0.5rem;
  margin-top: 0.5rem;
  font-size: small;

  .warning-title {
    color: #007acc;
    margin: 0;
  }

  a {
    color: #262626;
    cursor: pointer;
  }
`

const dismissClass = css`
  float: right;
  margin-bottom: 0.4rem;
`
