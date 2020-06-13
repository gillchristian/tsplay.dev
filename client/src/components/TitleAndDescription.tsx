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

    .strong {
      font-weight: 600;
    }

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
}

// NOTE: this should be added once the new version of the TypeScript website is released
const showPluginDescription = false

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

        {showPluginDescription && (
          <React.Fragment>
            <div css={styles.description}>Create short-links directly in the playground with the plugin:</div>
            <div css={styles.description}>
              <pre className="strong">
                <a
                  href="https://www.typescriptlang.org/play?install-plugin=typescript-playground-link-shortener"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  typescript-playground-link-shortener
                </a>
              </pre>
            </div>
          </React.Fragment>
        )}
        <div css={styles.description}>
          <strong className="strong">Why a dedicated link shortener?</strong> Did we mention short links? Ok, besides
          that, security is also a concern. <a href="https://tsplay.dev">tsplay.dev</a> links will always redirect to
          the TypeScript Playground. <i>You never know where bit.ly or tinyurl.com links could take you</i>
        </div>
      </div>
    </div>
  )
}

export default TitleAndDescription
