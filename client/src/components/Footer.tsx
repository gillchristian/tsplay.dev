/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import { Palette } from '../constants'

const styles = {
  footer: css`
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
  container: css`
    width: 100%;
    max-width: 720px;
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    justify-content: space-around;
  `,
  footerSection: css`
    width: 250px;
    margin-bottom: 10px;
    text-align: center;
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
    <div css={styles.footer}>
      <div css={styles.container}>
        <div css={styles.footerSection}>
          <span>by </span>
          <a href="https://dvnahuel.website/" rel="noopener noreferrer" target="_blank" css={styles.link}>
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
        <div css={styles.footerSection}>
          <a
            href="https://github.com/gillchristian/tsplay.dev"
            rel="noopener noreferrer"
            target="_blank"
            css={styles.link}
          >
            tsplay.dev on GitHub
          </a>
        </div>
      </div>
    </div>
  )
}

export default Footer
