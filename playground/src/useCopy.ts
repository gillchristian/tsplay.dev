import {useState} from 'react'
import * as TE from 'fp-ts/es6/TaskEither'
import * as T from 'fp-ts/es6/Task'
import {pipe} from 'fp-ts/es6/pipeable'

import {runTask, voidTask, delay, writeToClipboard} from './utils'

type CopyStatus = 'idle' | 'did_copy' | 'failure'

export const useCopy = () => {
  const [copyStatus, setStatus] = useState<CopyStatus>('idle')

  const onCopy = (text: string) => {
    if (copyStatus === 'did_copy') return

    pipe(
      text,
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

  return {copyStatus, onCopy}
}
