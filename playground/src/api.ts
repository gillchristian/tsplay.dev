import * as t from 'io-ts'
import * as E from 'fp-ts/es6/Either'
import * as TE from 'fp-ts/es6/TaskEither'
import {pipe} from 'fp-ts/es6/pipeable'
import {flow} from 'fp-ts/es6/function'

export interface CreateBody {
  url: string
  createShort?: string
}

export const CreateResponse = t.interface({shortened: t.string})
export interface CreateResponse extends t.TypeOf<typeof CreateResponse> {}

const decode = flow(
  CreateResponse.decode,
  E.mapLeft((): Errors => 'decoding'),
)

type InvalidUrl = 'invalid_url'
type ServerError = 'server_error'
type Unknown = 'unknown'
type Decoding = 'decoding'

export type Errors = InvalidUrl | ServerError | Unknown | Decoding

const statusToError = (status: number): Errors =>
  status === 400 ? 'invalid_url' : status === 500 ? 'server_error' : 'unknown'

// TODO use prod
const base = 'http://localhost:9000/'

const fetcher = (url: string, init?: RequestInit): Promise<unknown> =>
  fetch(url, init).then((res) =>
    res.ok ? res.json() : Promise.reject(res.status),
  )

export const createShort = (
  body: CreateBody,
): TE.TaskEither<Errors, CreateResponse> =>
  pipe(
    TE.tryCatch(
      () =>
        fetcher(`${base}create`, {
          method: 'post',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify(body),
        }),
      (error: unknown) => (typeof error === 'number' ? error : 500),
    ),
    TE.mapLeft(statusToError),
    TE.chainEitherK(decode),
  )
