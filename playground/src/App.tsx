import React, {FormEvent} from 'react'
import {css} from 'goober'
import {pipe} from 'fp-ts/es6/pipeable'
import * as TE from 'fp-ts/es6/TaskEither'
import * as T from 'fp-ts/es6/Task'
import * as O from 'fp-ts/es6/Option'

import {usePlugin} from './plugin'
import {createShort, Errors, CreateResponse} from './api'
import {RemoteData, isSuccess, isLoading} from './RemoteData'
import {runTask} from './utils'
import {useCopy} from './useCopy'

import {Failure} from './Failure'
import {Header} from './Header'
import {ShortenedLink} from './ShortenedLink'

const {useState} = React

interface CreatedLink {
  url: string
  code: string
}

type WIP = O.Option<{link: string; code: string}>

const urlWithCode = /(typescriptlang|staging-typescript)\.org\/.*code\/[A-Za-z0-9]+/
const hasEncodedCode = (url: string) => urlWithCode.test(url)

const matchesOtherLinks = (code: string, links: CreatedLink[]) =>
  links.some((link) => link.code === code.trim())

const shouldDisable = (
  activeCode: string,
  current: string,
  status: RemoteData,
  links: CreatedLink[],
) =>
  isLoading(status) ||
  (isSuccess(status) && current === status.prev) ||
  !hasEncodedCode(current) ||
  matchesOtherLinks(activeCode, links)

const App: React.FC = () => {
  const {code, setDebounce, flashInfo, setCode, markers} = usePlugin()

  const [links, setLinks] = useState<CreatedLink[]>([])
  const [status, setStatus] = useState<RemoteData>({type: 'idle'})
  const [wip, setWip] = useState<WIP>(O.none)

  const {copyStatus, onCopy} = useCopy()

  setDebounce(true)

  function viewLink(link: CreatedLink) {
    setCode(link.code)

    if (O.isNone(wip)) setWip(O.some({link: link.url, code}))
  }

  function restoreWip() {
    pipe(
      wip,
      O.fold(
        () => undefined,
        (prev) => setCode(prev.code),
      ),
    )

    setWip(O.none)
  }

  function createShortLink(e: FormEvent) {
    e.preventDefault()
    // eslint-disable-next-line no-restricted-globals
    const url = location.href
    const shortenedCode = code

    if (shouldDisable(code, url, status, links)) return

    setStatus({type: 'loading'})

    pipe(
      createShort({url}),
      TE.fold<Errors, CreateResponse, RemoteData>(
        (error) => T.of({type: 'failure', error}),
        ({shortened}) => T.of({type: 'success', shortened, prev: url}),
      ),
      T.map((newStatus) => {
        setStatus(newStatus)
        if (!isSuccess(newStatus)) return

        onCopy(newStatus.shortened)
        setLinks((prev) =>
          prev.concat([{url: newStatus.shortened, code: shortenedCode.trim()}]),
        )
        flashInfo('Short link created!')
      }),
      runTask,
    )
  }

  // eslint-disable-next-line no-restricted-globals
  const currentUrl = location.href

  // MarkerSeverity.Error === 8
  const hasErrors = markers.filter((v) => v.severity === 8).length > 0

  return (
    <div className={wrapperClass}>
      <Header />

      <div className={createClass}>
        <button
          className={`${createBtnClass} button`}
          onClick={createShortLink}
          disabled={shouldDisable(code, currentUrl, status, links)}
        >
          Create short link
        </button>

        {hasErrors && (
          <div className={failureClass + ' ' + textCenter}>
            <div>
              <small className="not-revert">There are erros in the editor</small>
            </div>
            <div>
              <small className="not-revert">Are you sure you want to crete a short link? <span>🤔</span></small>
            </div>
          </div>
        )}

        {!hasEncodedCode(currentUrl) && (
          <div className={warningClass + ' ' + textCenter}>
            <small className="not-revert">⚠️ No encoded code in the URL ⚠️</small>
          </div>
        )}

        {matchesOtherLinks(code, links) && (
          <div className={warningClass + ' ' + textCenter}>
            <small className="not-revert">⚠️ Already creted a short link for the current code ⚠️</small>
          </div>
        )}

        {copyStatus === 'did_copy' ? (
          <div className={successClass}>Copied <span className="not-revert">✅</span></div>
        ) : copyStatus === 'failure' ? (
          <div className={failureClass}>Cannot copy</div>
        ) : null}

        {status.type === 'failure' && <Failure error={status.error} />}
      </div>

      {links.length > 0 && (
        <>
          <h3 className={createdLinks}>Created links</h3>

          <div className={linksMsgClass}>
            <b>NOTE</b>: <i>Links are not persisted when reloading the page.</i>
          </div>

          <div className={linksMsgClass}>
            <b>View code</b> loads the code from the shortened link. The work in
            progress (WIP) is saved and can be restored.
          </div>
          <br />
          {links.map((link) => (
            <ShortenedLink
              link={link}
              active={wip}
              onView={viewLink}
              onRestoreWip={restoreWip}
            />
          ))}
        </>
      )}
    </div>
  )
}

const wrapperClass = css`
  padding: 10px;
  color: black;

  h1,
  h2,
  h3 {
    margin-top: 0.5rem !important;
    margin-bottom: 0.5rem !important;
  }

  background-color: #fff;

  .button {
    background: #007acc;
    text-transform: uppercase;
    align-items: center;
    cursor: pointer;
    padding: .3rem 1rem;
    color: #fff;
    text-decoration: none;
    font-size: .8rem;
    line-height: 1.15rem;
    border: none;
    outline: none;

    &:hover {
      background: #0583d8;
    }

    &:disabled {
    cursor: not-allowed;
    opacity: .6;
    background: #007acc !important;
    }
  }

  @media (prefers-color-scheme: dark) {
    filter: invert(100%) hue-rotate(180deg);

    .not-revert {
      filter: invert(100%) hue-rotate(180deg);
    }
  }
`

const createdLinks = css`
  font-weight: 600;
  font-size: 1.1rem;
`

const linksMsgClass = css`
  margin-bottom: 0.5rem;
  font-size: small;
`

const createBtnClass = css`
  margin-bottom: 0.8rem;
  font-size: .9rem !important;
  padding-bottom: .4rem !important;
`

const failureClass = css`
  color: #ad0606;
  font-weight: 600;
`

const warningClass = css`
  color: #f2c304;
  font-weight: 600;
`

const textCenter = css`
  text-align: center;
`

const createClass = css`
  margin: 0 auto;
  width: 100%;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
`

const successClass = css`
  color: #155724;
  font-weight: 600;
`

export default App
