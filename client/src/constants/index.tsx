export enum Palette {
  primary = '#007ACC',
  secondary = '#262626',
  shade1 = '#D8D8D8',
  shade2 = '#888',
  shade3 = '#494949',
  white = '#fff',
}
export const linksLocalStorageKey = 'tsplay.dev-links'
export const apiBaseUrl = process.env.NODE_ENV !== 'production' ? 'https://tsplay.dev/api' : 'https://tsplay.dev/api'
