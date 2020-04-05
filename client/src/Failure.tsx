import React, {FC} from 'react'

import {Errors} from './api'

export const Failure: FC<{error: Errors}> = ({error}) => (
  <div className="failure">
    {error === 'invalid_url' ? 'Invalid URL' : 'Something went wrong =/'}
  </div>
)
