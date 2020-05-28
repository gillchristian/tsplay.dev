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

    .strong {
      font-weight: 600;
    }
  `,
  description: css`
    font-size: 16px;
    line-height: 22px;

    @media (max-width: 1000px) {
      font-size: 15px;
    }
  `,
}

const TitleAndDescription: React.FC = () => {
  return (
    <div css={styles.container}>
      <img src={home} alt="home img" css={styles.img} />
      <div css={styles.descriptionWrapper}>
        <div css={styles.title}>
          TypeScript is <strong className="strong">Typed JavaScript at Any Scale.</strong>
        </div>
        <div css={styles.description}>
          TypeScript extends JavaScript by adding types to the language. TypeScript speeds up your development
          experience by catching errors and providing fixes before you even run your code. Any browser, any OS, anywhere
          JavaScript runs. Entirely Open Source.
        </div>
      </div>
    </div>
  )
}

export default TitleAndDescription
