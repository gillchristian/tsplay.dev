/** @jsx jsx */
import { css, jsx } from '@emotion/core'

import { ShowToast } from '../hooks/useCopyClipboardToast'

const styles = {
  underline: css`
    text-decoration: underline;
    margin-left: 5px;
  `,
}

export const copyToClipboard: (text: string, showToast: ShowToast) => void = async (text, showToast) => {
  try {
    await navigator.clipboard.writeText(text)
    showToast(
      <span>
        <span role="img" aria-label="check mark" className="prevent-hue-rotate">
          ✅{' '}
        </span>
        <strong css={styles.underline}>{text}</strong> copied to clipboard
      </span>
    )
  } catch (e) {
    showToast('⚠️ Sorry, there was an error trying to copy the link')
  }
}
