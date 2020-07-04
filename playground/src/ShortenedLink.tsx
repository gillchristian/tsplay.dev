import React, {FC} from 'react'
import {css} from 'goober'
import * as O from 'fp-ts/es6/Option'
import {pipe} from 'fp-ts/es6/pipeable'

import {useCopy} from './useCopy'

export interface CreatedLink {
  url: string
  code: string
}

interface Props {
  link: CreatedLink
  active: O.Option<{link: string}>
  onView: (link: CreatedLink) => void
  onRestoreWip: () => void
}

export const ShortenedLink: FC<Props> = ({
  link,
  active,
  onView,
  onRestoreWip,
}) => {
  const {copyStatus, onCopy} = useCopy()

  return (
    <div className={wrapperClass}>
      <div className={linkClass}>
        <a href={link.url} target="_blank" rel="noopener noreferrer">
          {link.url.replace(/^https?:\/\//, '')}
        </a>
      </div>

      <div className={colClass}>
        {pipe(
          active,
          O.fold(
            () => (
              <button
                className={`${buttonClass} button`}
                onClick={() => onView(link)}
              >
                View code
              </button>
            ),
            (activeLink) =>
              activeLink.link === link.url && (
                <button
                  className={`${buttonClass} button`}
                  onClick={onRestoreWip}
                >
                  Restore WIP
                </button>
              ),
          ),
        )}
      </div>

      <div className={colClass}>
        {copyStatus === 'idle' ? (
          <button
            className={`${buttonClass} button`}
            onClick={() => onCopy(link.url)}
          >
            Copy
          </button>
        ) : copyStatus === 'did_copy' ? (
          <span className={successClass}>
            Copied <span className="not-revert">✅</span>
          </span>
        ) : (
          <span className={failureClass}>Cannot not copy</span>
        )}
      </div>
    </div>
  )
}

// TODO: styles.ts
const buttonClass = css`
  cursor: pointer;
  &:disabled {
    cursor: not-allowed;
  }
`

const failureClass = css`
  color: #721c24;
`

const successClass = css`
  color: #155724;
`

const colClass = css`
  padding-right: 0.5rem;
`

const wrapperClass = css`
  display: flex;
  flex-wrap: wrap;
  justify-content: space-between;
  align-items: center;
  background-color: #d8d8d8;
  padding: 0.5rem;
  margin-right: -10px;
  margin-left: -10px;

  margin-bottom: 0.8rem;
  font-size: small;
`

const linkClass = css`
  padding-right: 0.5rem;
  width: 40%;
  min-width: 140px;
  padding-left: 0.5rem;

  a {
    color: #007acc !important;
  }
`
