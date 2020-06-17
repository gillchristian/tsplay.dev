/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import { Palette } from '../constants'
import wait from '../utils/wait'

const ANIMATION_TIME = 350 // ms

const styles = {
  container: css`
    height: 39px;
    width: 100%;
    font-size: 14px;
    display: flex;
    align-items: flex-end;
    width: 96%;
    position: relative;

    @media (max-width: 650px) {
      width: 95%;
    }
    @media (max-width: 500px) {
      width: 94%;
    }
    @media (max-width: 425px) {
      width: 93%;
    }
    @media (max-width: 375px) {
      width: 92%;
    }
    @media (max-width: 320px) {
      width: 90%;
    }
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
    transition: width ${ANIMATION_TIME}ms ease;
    position: relative;
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

    @media (max-width: 500px) {
      position: relative;
      top: 2px;
    }
  `,
  close: css`
    cursor: pointer;
    font-size: 22px;
    color: ${Palette.shade3};
    position: absolute;
    right: -25px;
    bottom: -2px;
  `,
}

interface Props {
  customLink: string
  setCustomLink: (customLink: string) => void
}

const CustomLinkString: React.FC<Props> = ({ customLink, setCustomLink }) => {
  const [customLinkMode, setCustomLinkMode] = React.useState(false)
  const [showCloseLinkMode, setShowCloseLinkMode] = React.useState(false)
  const [showToggleLinkMode, setShowToggleLinkMode] = React.useState(true)

  return (
    <div css={styles.container}>
      {showToggleLinkMode && (
        <div
          css={styles.trigger}
          role="button"
          onClick={async () => {
            setShowToggleLinkMode(false)
            setCustomLinkMode(!customLinkMode)
            await wait(ANIMATION_TIME)
            setShowCloseLinkMode(true)
          }}
        >
          Customize link back-half
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
          placeholder="my-ts-example"
          value={customLink}
          onChange={e => setCustomLink(e.target.value)}
        />
      </div>
      {showCloseLinkMode && (
        <span
          css={styles.close}
          role="button"
          onClick={async () => {
            setCustomLinkMode(false)
            setCustomLink('')
            setShowCloseLinkMode(false)
            await wait(ANIMATION_TIME + 150)
            setShowToggleLinkMode(true)
          }}
        >
          X
        </span>
      )}
    </div>
  )
}

export default CustomLinkString
