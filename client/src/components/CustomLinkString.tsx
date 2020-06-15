/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import { Palette } from '../constants'
import ClearInput from './ClearInput'

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
    display: flex;
    align-items: center;
    border-bottom: 2px solid ${Palette.secondary};
    overflow: hidden;
  `,
  label: css`
    font-size: 20px;
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
      {customLinkMode ? (
        <React.Fragment>
          <div css={styles.content}>
            <div css={styles.label}>tsplay.dev/</div>
            <input
              css={styles.input}
              placeholder="my-ts-example"
              value={customLink}
              onChange={e => setCustomLink(e.target.value)}
            />
          </div>
          <ClearInput
            onClear={() => {
              setCustomLinkMode(false)
              setCustomLink('')
            }}
          />
        </React.Fragment>
      ) : (
        <div css={styles.trigger} role="button" onClick={() => setCustomLinkMode(!customLinkMode)}>
          Customize link back-half
        </div>
      )}
    </div>
  )
}

export default CustomLinkString
