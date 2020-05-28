/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import { Palette } from '../constants'

const styles = {
  container: css`
    display: flex;
    align-items: center;
    margin-top: 30px;
  `,
  content: css`
    flex: 1;
    text-align: center;
    font-size: 25px;
    color: ${Palette.secondary};
    font-weight: 100;

    @media (max-width: 550px) {
      font-size: 20px;
    }

    span {
      margin-right: 8px;
      font-weight: bolder;
    }

    strong {
      font-weight: 100;
    }
  `,
}

const Stats: React.FC = () => {
  return (
    <div css={styles.container}>
      <div css={styles.content}>
        <span role="img" aria-label="emoji">
          👨🏻‍💻
        </span>
        Visits <strong> 50 </strong>
        <span role="img" aria-label="emoji">
          │
        </span>
        <span role="img" aria-label="emoji">
          🔗
        </span>
        Links <strong> 88 </strong>
      </div>
    </div>
  )
}

export default Stats
