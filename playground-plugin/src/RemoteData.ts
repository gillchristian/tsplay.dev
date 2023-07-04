import {Errors} from './api'

interface Idle {
  type: 'idle'
}
interface Loading {
  type: 'loading'
}
interface Success {
  type: 'success'
  shortened: string
  prev: string
}
interface Failure {
  type: 'failure'
  error: Errors
}

export type RemoteData = Idle | Loading | Success | Failure

export const isLoading = (rd: RemoteData): rd is Loading =>
  rd.type === 'loading'
export const isFailure = (rd: RemoteData): rd is Failure =>
  rd.type === 'failure'
export const isSuccess = (rd: RemoteData): rd is Success =>
  rd.type === 'success'
