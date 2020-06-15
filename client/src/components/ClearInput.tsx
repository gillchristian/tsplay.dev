/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import { Palette } from '../constants'

const styles = {
  clearInput: css`
    margin-left: 10px;
    cursor: pointer;
    font-size: 18px;
    color: ${Palette.shade3};
  `,
}

interface Props {
  onClear: () => void
}

const ClearInput: React.FC<Props> = ({ onClear }) => (
  <span css={styles.clearInput} role="button" onClick={onClear}>
    🅧
  </span>
)

export default ClearInput
