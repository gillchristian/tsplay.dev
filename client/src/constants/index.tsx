export enum Palette {
  primary = '#007ACC',
  secondary = '#262626',
  shade1 = '#D8D8D8',
  shade2 = '#888',
  shade3 = '#494949',
  white = '#fff',
  background = '#fffefe'
}
export const linksLocalStorageKey = 'tsplay.dev-links'
export const apiBaseUrl = process.env.NODE_ENV !== 'production' ? 'http://localhost:9000/api' : 'https://tsplay.dev/api'
