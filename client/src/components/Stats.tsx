/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import { Palette } from '../constants'

const styles = {
  container: css`
    display: flex;
    align-items: center;
    margin-top: 30px;
    min-height: 25px;

    @media (max-width: 550px) {
      min-height: 20px;
    }
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

interface Props {
  shortened: number | null
  visits: number | null
}

const Stats: React.FC<Props> = ({ shortened, visits }) => {
  return (
    <div css={styles.container}>
      <div css={styles.content}>
        {shortened && (
          <React.Fragment>
            <span role="img" aria-label="emoji">
              🔗
            </span>
            Created <strong> {shortened} </strong>
            <span role="img" aria-label="emoji">
              │
            </span>
          </React.Fragment>
        )}
        {visits && (
          <React.Fragment>
            <span role="img" aria-label="emoji">
              👩‍💻
            </span>
            Visited <strong> {visits} </strong>
          </React.Fragment>
        )}
      </div>
    </div>
  )
}

export default Stats
