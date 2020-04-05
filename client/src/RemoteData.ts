import {Errors} from './api'

export type RemoteData =
  | {type: 'idle'}
  | {type: 'loading'}
  | {type: 'success'; shortened: string; prev: string}
  | {type: 'failure'; error: Errors}

export const isLoading = ({type}: RemoteData) => type === 'loading'
export const isFailure = ({type}: RemoteData) => type === 'failure'
