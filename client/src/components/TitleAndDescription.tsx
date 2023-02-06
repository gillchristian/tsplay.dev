/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import home from '../assets/home.svg'
import { Palette } from '../constants'

const styles = {
  container: css`
    display: flex;
    align-items: center;
    margin-top: -10px;

    @media (max-width: 1000px) {
      margin-top: 0;
    }

    @media (max-width: 750px) {
      flex-direction: column;
    }
  `,
  img: css`
    order: 2;
    min-width: 336px;
    min-height: 224px;

    @media (max-width: 1000px) {
      min-width: 304px;
      min-height: 200px;
    }

    @media (max-width: 750px) {
      max-width: 200px;
    }
  `,
  descriptionWrapper: css`
    display: flex;
    color: ${Palette.secondary};
    flex-direction: column;
    padding: 0 30px 0 0;

    @media (max-width: 750px) {
      padding: 20px 0;
    }
  `,
  title: css`
    font-size: 38px;
    margin-bottom: 15px;

    @media (max-width: 1000px) {
      font-size: 28px;
    }
  `,
  description: css`
    margin-top: 10px;
    font-size: 16px;
    line-height: 22px;

    @media (max-width: 1000px) {
      font-size: 15px;
    }
  `,
  strong: css`
    font-weight: bold;
  `,
}

const TitleAndDescription: React.FC = () => {
  return (
    <div css={styles.container}>
      <img src={home} alt="home img" css={styles.img} className="prevent-hue-rotate" />
      <div css={styles.descriptionWrapper}>
        <div css={styles.title}>
          Short-links for the{' '}
          <strong className="strong">
            <a href="https://www.typescriptlang.org/play/" target="_blank" rel="noopener noreferrer">
              TypeScript playground
            </a>
          </strong>
          .
        </div>

        <div css={styles.description}>
          Easily share TypeScript snippets without having to send an URL that is longer than a <i>yarn.lock</i>.
        </div>

        <div css={styles.description}>
          <span css={styles.strong}>Why a dedicated link shortener?</span> Did we mention short links? Ok, besides that,
          security is also a concern. <a href="https://tsplay.dev">tsplay.dev</a> links will always redirect to the{' '}
          <a
            href="https://www.typescriptlang.org/play?install-plugin=typescript-playground-link-shortener"
            target="_blank"
            rel="noopener noreferrer"
            css={styles.strong}
          >
            TypeScript Playground
          </a>
          . <i>You never know where bit.ly or tinyurl.com links could take you</i>
        </div>

        <div css={styles.description}>
          To create short-links directly in the playground use the plugin:
          <a
            href="https://www.typescriptlang.org/play?install-plugin=typescript-playground-link-shortener"
            target="_blank"
            rel="noopener noreferrer"
            css={styles.strong}
          >
            typescript-playground-link-shortener
          </a>
        </div>

        <div css={styles.description}>
          <a
            href="https://www.buymeacoffee.com/gillchristian"
            target="_blank"
            rel="noopener noreferrer"
            css={styles.strong}
          >
            Buy me a burrito 🌯
          </a>{' '}
          to support tsplay.dev
        </div>
      </div>
    </div>
  )
}

export default TitleAndDescription
