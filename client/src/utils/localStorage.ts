import lscache from 'lscache'

// enable warnings
if (process.env.NODE_ENV !== 'production') {
  lscache.enableWarnings(true)
}

const storageKey = 'tsplay.dev'
interface LocalStorageInterface {
  set: (key: string, data: any, duration: number | undefined) => void
  get: (key: string) => any
  flush: () => void
}

const set = (key: string, data: any, duration: number | undefined = undefined) =>
  lscache.set(`${storageKey}-${key}`, data, duration)
const get = (key: string) => lscache.get(`${storageKey}-${key}`)
const flush = () => lscache.flush()

const localStorageService: LocalStorageInterface = { set, get, flush }

export default localStorageService
