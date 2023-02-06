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
  onDelete: (urlToDelete: string) => void
  onRestoreWip: () => void
}

export const ShortenedLink: FC<Props> = ({
  link,
  active,
  onView,
  onDelete,
  onRestoreWip,
}) => {
  const {copyStatus, onCopy} = useCopy()

  return (
    <div className={container}>
      <div className={linkClass}>
        <a href={link.url} target="_blank" rel="noopener noreferrer">
          {link.url.replace(/^https?:\/\//, '')}
        </a>
      </div>

      <div className={buttons}>
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
              Copied{' '}
              <span className="not-revert" role="img" aria-label="yes">
                ✅
              </span>
            </span>
          ) : (
            <span className={failureClass}>Cannot not copy</span>
          )}
        </div>

        {pipe(
          active,
          O.fold(
            () => (
              <div className={colClass}>
                <button
                  className={`${buttonClass} button`}
                  onClick={() => onView(link)}
                >
                  View code
                </button>
              </div>
            ),
            (activeLink) =>
              activeLink.link === link.url && (
                <div className={colClass}>
                  <button
                    className={`${buttonClass} button`}
                    onClick={onRestoreWip}
                  >
                    Restore WIP
                  </button>
                </div>
              ),
          ),
        )}

        <div className={colClass}>
          <button
            className={`${buttonClass} button`}
            onClick={() => onDelete(link.url)}
          >
            x
          </button>
        </div>
      </div>
    </div>
  )
}

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

const container = css`
  background-color: #d8d8d8;
  padding: 0.7rem;
  margin-bottom: 0.8rem;
`

const buttons = css`
  display: flex;
  flex-wrap: wrap;
  justify-content: space-between;
  align-items: center;

  font-size: small;
  margin-top: 10px;
`

const linkClass = css`
  font-size: 1.1rem;

  a {
    color: #007acc !important;
  }
`
