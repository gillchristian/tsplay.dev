import React, {FC} from 'react'
import {css} from 'goober'

import {Errors} from './api'

const failureClass = css`
  color: #721c24;
`
export const Failure: FC<{error: Errors}> = ({error}) => (
  <div className={failureClass}>
    {error === 'invalid_url' ? 'Invalid URL' : 'Something went wrong =/'}
  </div>
)
