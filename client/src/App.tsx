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

const MAX_WIDTH = 800

const styles = {
  container: css`
    min-height: 100vh;
    background: ${Palette.white};
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

const links = [
  'tsplay.dev/Hm4ywj',
  'tsplay.dev/react-hooks',
  'tsplay.dev/W4ywjm',
  'tsplay.dev/3aSa3d',
  'tsplay.dev/ts-tuto-1',
  'tsplay.dev/ts-tuto-2',
  'tsplay.dev/mYpGng',
  'tsplay.dev/yAmVwl',
  'tsplay.dev/Wv8jVm',
  'tsplay.dev/generics-example',
  'tsplay.dev/qjWwVG',
  'tsplay.dev/2pLmWY',
]

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

  const showToast = useCopyClipboardToast()

  const setShortenedCreatedArray = React.useMemo(() => [shortenedCreated], [shortenedCreated])

  // useMemo runs before useEffect (like the class constructor)
  React.useMemo(async () => {
    const stats = await fetchStats()

    if (stats) {
      setShortened(stats.totalShortened)
      setVisits(stats.totalVisits)
    }
  }, [])

  return (
    <div css={styles.container}>
      <Header />
      <div css={styles.content}>
        <TitleAndDescription />
        <Stats shortened={shortened} visits={visits} />
        <LinkCreator setShortened={setShortened} setShortenedCreated={setShortenedCreated} showToast={showToast} />
        {shortenedCreated && <Links links={setShortenedCreatedArray} canDeleteItem={false} showToast={showToast} />}
        <Links links={links} canDeleteItem={true} showToast={showToast} />
      </div>
      <Footer />
    </div>
  )
}

export default App
