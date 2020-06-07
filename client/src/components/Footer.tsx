/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import { Palette } from '../constants'

const styles = {
  container: css`
    height: 50px;
    width: 100vw;
    font-size: 14px;
    margin: 30px auto 0;
    color: ${Palette.secondary};
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
  `,
  link: css`
    text-decoration: underline;
    cursor: pointer;
    color: ${Palette.secondary};

    &:hover {
      color: ${Palette.primary};
    }
  `,
}

const Footer: React.FC = () => {
  return (
    <div css={styles.container}>
      <div>
        <span>by </span>
        <a href="https://dvnahuel.site/" rel="noopener noreferrer" target="_blank" css={styles.link}>
          dvnahuel
        </a>
        <span> / </span>
        <a href="https://gillchristian.xyz/" rel="noopener noreferrer" target="_blank" css={styles.link}>
          gillchristian
        </a>
        <span> / </span>
        <a href="https://jonidelv.me/" rel="noopener noreferrer" target="_blank" css={styles.link}>
          jonidelv
        </a>
      </div>
    </div>
  )
}

export default Footer
