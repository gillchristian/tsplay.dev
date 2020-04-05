import React, {FC, FormEvent, ChangeEvent, useState} from 'react'
import * as TE from 'fp-ts/lib/TaskEither'
import * as T from 'fp-ts/lib/Task'
import {pipe} from 'fp-ts/lib/pipeable'
import isUrl from 'is-url'

import './App.css'

import {createShort, Errors, CreateResponse} from './api'
import {runTask} from './util'
import {Result} from './Result'
import {RemoteData, isLoading, isFailure} from './RemoteData'

const shouldDisable = (current: string, status: RemoteData) =>
  status.type === 'success'
    ? current === status.prev
    : status.type === 'loading'
    ? true
    : isUrl(current)
    ? !/typescriptlang.org/.test(current)
    : true

export const App: FC = () => {
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
