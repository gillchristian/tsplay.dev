/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import { Palette } from '../constants'

const styles = {
  container: css`
    height: 15px;
    display: flex;
    align-items: center;
    padding: 0 15px;
    justify-content: center;
    font-size: 14px;
    margin-top: 30px;
    color: ${Palette.secondary};

    @media (max-width: 1000px) {
      margin: 20px 0;
    }
  `,
  link: css`
    text-decoration: underline;
    margin-left: 5px;
    cursor: pointer;
    color: ${Palette.secondary};

    &:hover {
      color: ${Palette.primary};
    }
  `,
  margin: css`
    margin-left: 5px;
    &.emoji {
      margin-right: 5px;
    }
  `,
}

const Header: React.FC = () => {
  return (
    <div css={styles.container}>
      Created with
      <span role="img" aria-label="emoji" css={styles.margin} className="emoji">
        🖤
      </span>
      by
      <a href="https://gillchristian.xyz/" rel="noopener noreferrer" target="_blank" css={styles.link}>
        gillchristian
      </a>
      ,
      <a href="https://jonidelv.me/" rel="noopener noreferrer" target="_blank" css={styles.link}>
        jonidelv
      </a>
      <span css={styles.margin}>and</span>
      <a href="https://dvnahuel.site/" rel="noopener noreferrer" target="_blank" css={styles.link}>
        dvnahuel
      </a>
    </div>
  )
}

export default Header
