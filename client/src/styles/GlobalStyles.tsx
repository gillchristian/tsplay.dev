/** @jsx jsx */
import * as React from 'react'
import { css, jsx, Global, SerializedStyles } from '@emotion/core'
import Fonts from './Fonts'
import { Palette } from '../constants'

export const SCROLL_BAR_WIDTH = 15

const globalStyles: SerializedStyles = css`
  @import url('https://fonts.googleapis.com/css?family=Montserrat:200,400,500');

  html,
  body,
  div,
  span,
  h1,
  h2,
  h3,
  h4,
  h5,
  h6,
  p,
  blockquote,
  pre,
  a,
  cite,
  code,
  em,
  img,
  strong,
  ol,
  ul,
  li,
  fieldset,
  form,
  label,
  legend,
  table,
  caption,
  tbody,
  tfoot,
  thead,
  tr,
  th,
  td,
  article,
  aside,
  canvas,
  details,
  embed,
  figure,
  footer,
  header,
  menu,
  nav,
  output,
  section,
  time,
  mark,
  audio,
  video {
    margin: 0;
    padding: 0;
    border: 0;
    font-size: 100%;
    vertical-align: baseline;
  }

  article,
  aside,
  details,
  figcaption,
  figure,
  footer,
  header,
  hgroup,
  menu,
  nav,
  section {
    display: block;
  }

  html,
  body,
  #root {
    line-height: 1;
    height: 100%;
    width: 100%;
    -moz-osx-font-smoothing: grayscale;
    -webkit-font-smoothing: antialiased;
    font-family: 'Segoe', Segoe UI, -apple-system, BlinkMacSystemFont, Roboto, Helvetica Neue, sans-serif;
    overflow-x: hidden;

    ::-webkit-scrollbar {
      width: ${SCROLL_BAR_WIDTH}px;
    }

    ::-webkit-scrollbar-track {
      background: ${Palette.shade3};
    }

    ::-webkit-scrollbar-thumb {
      background: ${Palette.shade2};
    }
  }

  body button,
  body input,
  body textarea {
    font-family: inherit;
  }

  ol,
  ul {
    list-style: none;
  }

  blockquote,
  q {
    quotes: none;
  }

  table {
    border-collapse: collapse;
    border-spacing: 0;
  }

  a {
    color: inherit;
    text-decoration: none;
  }

  strong {
    font-weight: 600;
  }
`

const GlobalStyles: React.FC = () => {
  return (
    <React.Fragment>
      <Fonts />
      <Global styles={globalStyles} />
    </React.Fragment>
  )
}

export default GlobalStyles
