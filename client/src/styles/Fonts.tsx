/** @jsx jsx */
import React from 'react'
import { css, jsx, Global, SerializedStyles } from '@emotion/core'

const fonts: SerializedStyles = css`
  @font-face {
    font-family: 'Segoe';
    src: url(https://static2.sharepointonline.com/files/fabric/assets/fonts/segoeui-westeuropean/segoeui-light.woff2)
        format('woff2'),
      url(https://static2.sharepointonline.com/files/fabric/assets/fonts/segoeui-westeuropean/segoeui-light.woff)
        format('woff');
    font-weight: 100;
    font-style: normal;
  }

  @font-face {
    font-family: 'Segoe';
    src: url(https://static2.sharepointonline.com/files/fabric/assets/fonts/segoeui-westeuropean/segoeui-semilight.woff2)
        format('woff2'),
      url(https://static2.sharepointonline.com/files/fabric/assets/fonts/segoeui-westeuropean/segoeui-semilight.woff)
        format('woff');
    font-weight: 300;
    font-style: normal;
  }

  @font-face {
    font-family: 'Segoe';
    src: url(https://static2.sharepointonline.com/files/fabric/assets/fonts/segoeui-westeuropean/segoeui-regular.woff2)
        format('woff2'),
      url(https://static2.sharepointonline.com/files/fabric/assets/fonts/segoeui-westeuropean/segoeui-regular.woff)
        format('woff');
    font-weight: 400;
    font-style: normal;
  }

  @font-face {
    font-family: 'Segoe';
    src: url(https://static2.sharepointonline.com/files/fabric/assets/fonts/segoeui-westeuropean/segoeui-semibold.woff2)
        format('woff2'),
      url(https://static2.sharepointonline.com/files/fabric/assets/fonts/segoeui-westeuropean/segoeui-semibold.woff)
        format('woff');
    font-weight: 600;
    font-style: normal;
  }

  @font-face {
    font-family: 'Segoe';
    src: url(https://static2.sharepointonline.com/files/fabric/assets/fonts/segoeui-westeuropean/segoeui-bold.woff2)
        format('woff2'),
      url(https://static2.sharepointonline.com/files/fabric/assets/fonts/segoeui-westeuropean/segoeui-bold.woff)
        format('woff');
    font-weight: 700;
    font-style: normal;
  }
`

const Fonts: React.FC = () => {
  return <Global styles={fonts} />
}

export default Fonts
