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

const M = 1000000
const k = 1000

const toFixed = (n: number, digits: number) => Number.parseFloat(n.toFixed(digits))

const formatStat = (stat: number) =>
  stat > M - 1 ? `${toFixed(stat / M, 2)}M` : stat > k - 1 ? `${toFixed(stat / M, 1)}k` : stat.toString()

const Stats: React.FC<Props> = ({ shortened, visits }) => {
  return (
    <div css={styles.container}>
      <div css={styles.content}>
        {shortened && (
          <React.Fragment>
            <span role="img" aria-label="emoji" className="prevent-hue-rotate">
              🔗
            </span>
            Created <strong> {formatStat(shortened)} </strong>
          </React.Fragment>
        )}
        {Boolean(shortened) && Boolean(visits) && <span>│</span>}
        {visits && (
          <React.Fragment>
            <span role="img" aria-label="emoji" className="prevent-hue-rotate">
              👩‍💻
            </span>
            Visited <strong> {formatStat(visits)} </strong>
          </React.Fragment>
        )}
      </div>
    </div>
  )
}

export default Stats
