/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import { lighten } from 'polished'
import { Palette } from '../constants'

export const CONTAINER_HEIGHT = 50

const styles = {
  container: css`
    display: flex;
    margin-top: 35px;
    justify-content: center;
    align-items: center;

    @media (max-width: 550px) {
      margin-top: 27px;
    }
  `,
  input: css`
    width: 85%;
    height: ${CONTAINER_HEIGHT}px;
    background: ${Palette.shade1};
    outline: none;
    padding: 8px 15px;
    box-sizing: border-box;
    border: none;
    color: ${Palette.secondary};
    font-size: 22px;

    @media (max-width: 550px) {
      width: 90%;
    }
  `,
  button: css`
    height: ${CONTAINER_HEIGHT}px;
    padding: 0 30px;
    font-size: 20px;
    text-transform: uppercase;
    background: ${Palette.primary};
    border: none;
    color: ${Palette.white};
    flex: 1;
    display: flex;
    justify-content: center;
    align-items: center;
    outline: none;
    cursor: pointer;

    @media (max-width: 550px) {
      padding: 0 12px;
      font-size: 15px;
    }

    &:hover {
      background: ${lighten(0.05, Palette.primary)};
    }
  `,
}

const LinkCreator: React.FC = () => {
  return (
    <div css={styles.container}>
      <input type="text" css={styles.input} />
      <button css={styles.button}>Shorten</button>
    </div>
  )
}

export default LinkCreator
