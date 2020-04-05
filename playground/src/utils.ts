import * as T from 'fp-ts/es6/Task'
import * as E from 'fp-ts/es6/Either'
import * as TE from 'fp-ts/es6/TaskEither'
import {pipe} from 'fp-ts/es6/pipeable'
import {constVoid} from 'fp-ts/es6/function'

export function runTask<A = unknown>(task: T.Task<A>) {
  return task()
}

export const voidTask: T.Task<void> = T.of(undefined)

export const delay = (ms: number) => () => T.delay(ms)(voidTask)


export const writeToClipboard = (str: string) =>
  pipe(
    E.tryCatch(() => navigator.clipboard.writeText(str), constVoid),
    TE.fromEither,
    TE.chain((p) => TE.tryCatch(() => p, constVoid)),
  )
