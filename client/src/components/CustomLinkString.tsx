/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import { Palette } from '../constants'

const styles = {
  container: css`
    height: 39px;
    width: 100%;
    font-size: 14px;
    display: flex;
    align-items: flex-end;
  `,
  trigger: css`
    text-decoration: underline;
    font-size: 16px;
    cursor: pointer;
    position: relative;
    bottom: 3px;

    &:hover {
      color: ${Palette.primary};
    }
  `,
  content: css`
    width: 100%;
    transition: width 0.3s ease;
    display: flex;
    align-items: center;
    border-bottom: 2px solid ${Palette.secondary};
    overflow: hidden;
  `,
  label: css`
    font-size: 20px;
  `,
  inputContainer: css`
    overflow: hidden;
    transition: width 0.3s ease;
    position: relative;
    top: 1px;
  `,
  input: css`
    border: none;
    outline: none;
    width: 100%;
    padding: 0 0 0 1px;
    font-size: 19px;
    line-height: 19px;
  `,
}

interface Props {
  customLink: string
  setCustomLink: (customLink: string) => void
}

const CustomLinkString: React.FC<Props> = ({ customLink, setCustomLink }) => {
  const [customLinkMode, setCustomLinkMode] = React.useState(false)

  return (
    <div css={styles.container}>
      {!customLinkMode && (
        <div css={styles.trigger} role="button" onClick={() => setCustomLinkMode(!customLinkMode)}>
          Add a custom link
        </div>
      )}
      <div
        css={css`
          ${styles.content};
          width: ${customLinkMode ? '100%' : '0px'};
        `}
      >
        <div css={styles.label}>tsplay.dev/</div>
        <input
          css={styles.input}
          placeholder="custom link here"
          value={customLink}
          onChange={e => setCustomLink(e.target.value)}
        />
      </div>
    </div>
  )
}

export default CustomLinkString
