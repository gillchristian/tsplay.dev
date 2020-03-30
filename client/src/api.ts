import * as t from 'io-ts'
import * as E from 'fp-ts/lib/Either'
import * as TE from 'fp-ts/lib/TaskEither'
import * as O from 'fp-ts/lib/Option'
import {pipe} from 'fp-ts/lib/pipeable'
import {Fetcher} from 'fetcher-ts'

export interface CreateBody {
  url: string
  createShort?: string
}

export const CreateResponse = t.interface({shortened: t.string})
export type CreateResponse = t.TypeOf<typeof CreateResponse>

type InvalidUrl = 'invalid_url'
type ServerError = 'server_error'
type Unknown = 'unknown'
type Decoding = 'decoding'

export type Errors = InvalidUrl | ServerError | Unknown | Decoding

type CreateResult =
  | {code: 200; payload: CreateResponse}
  | {code: 201; payload: CreateResponse}
  | {code: 400; payload: Error}
  | {code: 500; payload: Error}

const base = process.env.REACT_APP_BASE_URL || 'http://localhost:9000/'

type Result = E.Either<Errors, CreateResponse>

export const createShort = (body: CreateBody) =>
  pipe(
    new Fetcher<CreateResult, Result>(`${base}create`, {
      method: 'post',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify(body),
    })
      .handle(200, E.right, CreateResponse)
      .handle(201, E.right, CreateResponse)
      .handle(400, () => E.left('invalid_url'), t.any)
      .handle(500, () => E.left('server_error'), t.any)
      .discardRest(() => E.left('unknown'))
      .toTaskEither(),
    TE.mapLeft<Error, Errors>(() => 'unknown'),
    TE.chain(([eRes, mbDecoding]) =>
      O.fold(
        () => TE.fromEither(eRes),
        () => TE.fromEither<Errors, never>(E.left('decoding')),
      )(mbDecoding),
    ),
  )
