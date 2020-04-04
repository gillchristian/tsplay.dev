import React, {FC, FormEvent, ChangeEvent, useState, useEffect} from 'react'
import * as E from 'fp-ts/lib/Either'
import * as TE from 'fp-ts/lib/TaskEither'
import * as T from 'fp-ts/lib/Task'
import {pipe} from 'fp-ts/lib/pipeable'
import {constVoid} from 'fp-ts/lib/function'
import isUrl from 'is-url'
import cx from 'classnames'

import './App.css'

import {createShort, Errors, CreateResponse} from './api'

type RemoteData =
  | {type: 'idle'}
  | {type: 'loading'}
  | {type: 'success'; shortened: string; prev: string}
  | {type: 'failure'; error: Errors}

const isLoading = ({type}: RemoteData) => type === 'loading'
const isFailure = ({type}: RemoteData) => type === 'failure'

const shouldDisable = (current: string, status: RemoteData) =>
  status.type === 'success'
    ? current === status.prev
    : status.type === 'loading'
    ? true
    : isUrl(current)
    ? !/typescriptlang.org/.test(current)
    : true

const Failure: FC<{error: Errors}> = ({error}) => (
  <div className="failure">
    {error === 'invalid_url' ? 'Invalid URL' : 'Something went wrong =/'}
  </div>
)

const voidTask: T.Task<void> = T.of(undefined)

const writeToClipboard = (str: string) =>
  pipe(
    E.tryCatch(() => navigator.clipboard.writeText(str), constVoid),
    TE.fromEither,
    TE.chain((p) => TE.tryCatch(() => p, constVoid)),
  )

function runTask<A = unknown>(task: T.Task<A>) {
  return task()
}

const delay = (ms: number) => () => T.delay(ms)(voidTask)

const Result: FC<{result: RemoteData}> = ({result}) => {
  type CopyStatus = 'idle' | 'did_copy' | 'failure'
  const [copyStatus, setStatus] = useState<CopyStatus>('idle')

  useEffect(() => {
    onCopy()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [result.type])

  const onCopy = () => {
    if (result.type !== 'success' || copyStatus === 'did_copy') return

    pipe(
      result.shortened,
      writeToClipboard,
      TE.fold(
        () => T.task.map(voidTask, () => setStatus('failure')),
        () => T.task.map(voidTask, () => setStatus('did_copy')),
      ),
      T.chain(delay(2500)),
      T.map(() => setStatus('idle')),
      runTask,
    )
  }

  return (
    <div className="result">
      {result.type === 'success' ? (
        <div className="success" onClick={onCopy}>
          <div>Short link created!</div>
          <div className="link">
            {result.shortened.replace(/https?:\/\//, '')}
          </div>
          <div className={cx('copy', {failure: copyStatus === 'failure'})}>
            {copyStatus === 'did_copy'
              ? 'Copied!'
              : copyStatus === 'idle'
              ? 'Copy'
              : 'Cannot copy'}
          </div>
        </div>
      ) : result.type === 'failure' ? (
        <Failure error={result.error} />
      ) : null}
    </div>
  )
}

export const App = () => {
  const [url, setUrl] = useState('')
  const [status, setStatus] = useState<RemoteData>({type: 'idle'})

  const create = (e: FormEvent) => {
    e.preventDefault()

    if (shouldDisable(url, status)) return

    setStatus({type: 'loading'})

    pipe(
      createShort({url}),
      TE.fold<Errors, CreateResponse, RemoteData>(
        (error) => T.of({type: 'failure', error}),
        ({shortened}) => T.of({type: 'success', shortened, prev: url}),
      ),
      runTask,
      (p) => p.then(setStatus),
    )
  }

  const onUrlChange = (e: ChangeEvent<HTMLInputElement>) => {
    if (isLoading(status)) return
    if (isFailure(status)) setStatus({type: 'idle'})

    setUrl(e.currentTarget.value)
  }

  return (
    <div className="App">
      <form className="shorten" onSubmit={create}>
        <input
          className="shorten-input"
          name="url"
          value={url}
          onChange={onUrlChange}
          disabled={isLoading(status)}
          placeholder="TypeScript playground link"
          autoFocus
        />
        <button
          type="submit"
          className="shorten-button"
          disabled={shouldDisable(url, status)}
        >
          Shorten
        </button>
      </form>

      <Result result={status} />
    </div>
  )
}
