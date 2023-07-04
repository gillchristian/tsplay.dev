import {pipe} from 'fp-ts/es6/pipeable'
import {flow} from 'fp-ts/es6/function'
import * as E from 'fp-ts/es6/Either'
import * as IOE from 'fp-ts/es6/IOEither'
import * as IO from 'fp-ts/es6/IO'
import * as T from 'fp-ts/es6/Task'
import * as O from 'fp-ts/es6/Option'
import * as storage from 'fp-ts-local-storage'
import * as t from 'io-ts'

const STORAGE_KEY = '[tsplay.dev] created'

const CreatedLink = t.interface({url: t.string, code: t.string})

export interface CreatedLink extends t.TypeOf<typeof CreatedLink> {}

const Links = t.array(CreatedLink)

const decodeLinks = flow(
  Links.decode,
  E.mapLeft(() => new Error('Invalid json in localStorage')),
)

export const readLinks = pipe(
  IOE.tryCatch(storage.getItem(STORAGE_KEY), E.toError),
  IOE.chain(
    flow(
      O.getOrElse(() => '[]'),
      (s) => E.parseJSON(s, E.toError),
      E.chain(decodeLinks),
      IOE.fromEither,
    ),
  ),
)

const persistLinks = flow(
  IOE.rightIO,
  IOE.chain((updatedItems) =>
    IOE.tryCatch(
      storage.setItem(STORAGE_KEY, JSON.stringify(updatedItems)),
      E.toError,
    ),
  ),
  IOE.fold((error) => () => console.log(error), IO.of),
  T.fromIO,
)

export const updateLinks = (newLink: CreatedLink) =>
  pipe(
    readLinks,
    IOE.map((parsedLinks) => parsedLinks.concat([newLink])),
    IOE.fold(() => IO.of([]), IO.of),
    persistLinks,
  )

export const deleteLink = (urlToDelete: string) =>
  pipe(
    readLinks,
    IOE.map((parsedLinks) =>
      parsedLinks.filter((stored) => stored.url !== urlToDelete),
    ),
    IOE.fold(() => IO.of([]), IO.of),
    persistLinks,
  )
