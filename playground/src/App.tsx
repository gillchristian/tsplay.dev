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
          className={createBtnClass}
          onClick={createShortLink}
          disabled={shouldDisable(code, currentUrl, status, links)}
        >
          Create short link
        </button>

        {hasErrors && (
          <div className={failureClass + ' ' + textCenter}>
            <div>
              <small>There are erros in the editor</small>
            </div>
            <div>
              <small>Are you sure you want to crete a short link?</small>
            </div>
          </div>
        )}

        {!hasEncodedCode(currentUrl) && (
          <div className={warningClass + ' ' + textCenter}>
            <small>No encoded code in the URL</small>
          </div>
        )}

        {matchesOtherLinks(code, links) && (
          <div className={warningClass + ' ' + textCenter}>
            <small>Already creted a short link for the current code</small>
          </div>
        )}

        {copyStatus === 'did_copy' ? (
          <div className={successClass}>Copied!</div>
        ) : copyStatus === 'failure' ? (
          <div className={failureClass}>Cannot copy</div>
        ) : null}

        {status.type === 'failure' && <Failure error={status.error} />}
      </div>

      {links.length > 0 && (
        <>
          <h3>Created links</h3>

          <div className={linksMsgClass}>
            <b>NOTE</b>: <i>Links are not persisted when reloading the page.</i>
          </div>

          <div className={linksMsgClass}>
            <b>View code</b> loads the code from the shortened link. The work in
            progress (WIP) is saved and can be restored.
          </div>

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
`

const linksMsgClass = css`
  margin-bottom: 0.5rem;
  font-size: small;
`

const createBtnClass = css`
  font-size: 1.1rem;
  margin-bottom: 0.5rem;

  cursor: pointer;
  &:disabled {
    cursor: not-allowed;
  }
`

const failureClass = css`
  color: #721c24;
`

const warningClass = css`
  color: #856404;
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
`

export default App
