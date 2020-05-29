/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import Header from './components/Header'
import TitleAndDescription from './components/TitleAndDescription'
import Stats from './components/Stats'
import LinkCreator from './components/LinkCreator'
import Links from './components/Links'
import Footer from './components/Footer'
import { Palette } from './constants'

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

const App: React.FC = () => {
  return (
    <div css={styles.container}>
      <Header />
      <div css={styles.content}>
        <TitleAndDescription />
        <Stats />
        <LinkCreator />
        <Links links={[links[0]]} canDeleteItem={false} />
        <Links links={links} canDeleteItem={true} />
      </div>
      <Footer />
    </div>
  )
}

export default App
