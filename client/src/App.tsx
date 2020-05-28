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
    height: 100%;
  `,
  content: css`
    max-width: ${MAX_WIDTH}px;
    margin: auto;

    @media (max-width: 850px) {
      padding: 0 20px;
    }
  `,
  wrapper: css`
    background: ${Palette.white};
  `,
}

const links = [
  'https://www.newlinkto.com/ass312dasd',
  'https://www.newlinkto.com/wqeasasdasdas',
  'https://www.newlinkto.com/33asasd',
  'https://www.newlinkto.com/asoqaswe.sad',
  'https://www.newlinkto.com/asoqwe.saad',
  'https://www.newlinkto.com/asoqw123e.sad',
  'https://www.newlinkto.com/asoqdawe.sad',
  'https://www.newlinkto.com/asoq12we.sad',
  'https://www.newlinkto.com/asoqwe.sad',
  'https://www.newlinkto.com/asoasdxqwe.sad',
  'https://www.newlinkto.com/asoqasawe.sad',
  'https://www.newlinkto.com/asozqwe.sad',
  'https://www.newlinkto.com/aso43qwe.sad',
]

const App: React.FC = () => {
  return (
    <div css={styles.container}>
      <div css={styles.wrapper}>
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
    </div>
  )
}

export default App
