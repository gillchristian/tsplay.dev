/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import Header from './components/Header'
import TitleAndDescription from './components/TitleAndDescription'
import Stats from './components/Stats'
import LinkCreator from './components/LinkCreator'
import Links from './components/Links'
import Footer from './components/Footer'
import { useCopyClipboardToast } from './hooks/useCopyClipboardToast'
import { Palette } from './constants'
import api from './utils/api'
import { linksLocalStorageKey } from './constants'
import localStorage from './utils/localStorage'

const MAX_WIDTH = 800

const styles = {
  container: css`
    min-height: 100vh;
    background: ${Palette.background};
    display: flex;
    flex-direction: column;
  `,
  content: css`
    flex: 1;
    max-width: ${MAX_WIDTH}px;
    margin: 20px auto 0;

    @media (max-width: 850px) {
      padding: 0 20px;
    }
  `,
}

const getLinksFromLocalStorage = () => {
  const links: string[] = localStorage.get(linksLocalStorageKey) || []
  return links
}

export type StatsResponse = {
  totalShortened: number | null
  totalVisits: number | null
}

const fetchStats = async () => {
  try {
    return await api<StatsResponse>('short/stats')
  } catch (error) {
    // eslint-disable-next-line no-console
    console.log('Error fetching stats')
    // eslint-disable-next-line no-console
    console.log(error)
  }
}

const App: React.FC = () => {
  const [shortened, setShortened] = React.useState<number | null>(null)
  const [visits, setVisits] = React.useState<number | null>(null)
  const [shortenedCreated, setShortenedCreated] = React.useState('')
  const [links, setLinks] = React.useState<string[]>([])

  const showToast = useCopyClipboardToast()

  const setShortenedCreatedArray = React.useMemo(() => [shortenedCreated], [shortenedCreated])

  // useMemo runs before useEffect (like the class constructor)
  React.useMemo(async () => {
    const links: string[] = getLinksFromLocalStorage()
    if (links.length) setLinks(links)

    const stats = await fetchStats()
    if (stats) {
      setShortened(stats.totalShortened)
      setVisits(stats.totalVisits)
    }
  }, [])

  const onLinkDelete = React.useCallback((url: string, links: string[]) => {
    const linksFiltered = links.filter(link => link !== url)
    localStorage.set(linksLocalStorageKey, linksFiltered)
    setLinks(linksFiltered)
  }, [])

  const linksFilteringCreated = React.useMemo(() => links.filter(link => link !== shortenedCreated), [
    links,
    shortenedCreated,
  ])

  return (
    <div css={styles.container}>
      <Header />
      <div css={styles.content}>
        <TitleAndDescription />
        <Stats shortened={shortened} visits={visits} />
        <LinkCreator
          setShortened={setShortened}
          setShortenedCreated={setShortenedCreated}
          showToast={showToast}
          setLinks={setLinks}
        />
        {shortenedCreated && <Links links={setShortenedCreatedArray} canDeleteItem={false} showToast={showToast} />}
        {linksFilteringCreated.length > 0 && (
          <Links links={linksFilteringCreated} canDeleteItem={true} showToast={showToast} onLinkDelete={onLinkDelete} />
        )}
      </div>
      <Footer />
    </div>
  )
}

export default App
