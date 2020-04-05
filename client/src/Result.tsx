import React, {FC, useState, useEffect} from 'react'
import * as TE from 'fp-ts/lib/TaskEither'
import * as T from 'fp-ts/lib/Task'
import {pipe} from 'fp-ts/lib/pipeable'
import cx from 'classnames'

import {Failure} from './Failure'
import {runTask, voidTask, delay, writeToClipboard} from './util'
import {RemoteData} from './RemoteData'

export const Result: FC<{result: RemoteData}> = ({result}) => {
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
