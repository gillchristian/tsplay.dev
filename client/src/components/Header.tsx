/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import logoImg from '../assets/logo.svg'
import { Palette } from '../constants'

const styles = {
  container: css`
    background: transparent;
    height: 70px;
    display: flex;
    padding: 0 25px;
    align-items: flex-end;
  `,
  logo: css`
    height: 45px;
    width: 45px;
  `,
  title: css`
    font-size: 2rem;
    color: ${Palette.secondary};
    margin-left: 10px;
    font-weight: 600;
  `,
}

const Header: React.FC = () => {
  return (
    <div css={styles.container}>
      <img src={logoImg} alt="logo" css={styles.logo} className="prevent-hue-rotate" />
      <span css={styles.title}>tsplay.dev</span>
    </div>
  )
}

export default Header
