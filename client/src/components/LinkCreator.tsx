/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import { lighten } from 'polished'
import { Palette } from '../constants'
import { toast } from 'react-toastify'
import api from '../utils/api'
import { ShowToast } from '../hooks/useCopyClipboardToast'

export const CONTAINER_HEIGHT = 50

const styles = {
  container: css`
    display: flex;
    margin-top: 35px;
    justify-content: center;
    align-items: center;

    @media (max-width: 550px) {
      margin-top: 27px;
    }
  `,
  inputContainer: css`
    position: relative;
    width: 80%;
    height: ${CONTAINER_HEIGHT}px;
    box-sizing: border-box;

    @media (max-width: 550px) {
      width: 90%;
    }
  `,
  input: css`
    width: 100%;
    height: ${CONTAINER_HEIGHT}px;
    background: ${Palette.shade1};
    outline: none;
    padding: 10px 20px;
    padding-right: 30px;
    box-sizing: border-box;
    border: none;
    color: ${Palette.secondary};
    font-size: 22px;
  `,
  button: css`
    height: ${CONTAINER_HEIGHT}px;
    padding: 0 30px;
    font-size: 20px;
    text-transform: uppercase;
    background: ${Palette.primary};
    border: none;
    color: ${Palette.white};
    flex: 1;
    display: flex;
    justify-content: center;
    align-items: center;
    outline: none;
    cursor: pointer;

    @media (max-width: 550px) {
      padding: 0 12px;
      font-size: 15px;
    }

    &:hover {
      background: ${lighten(0.05, Palette.primary)};
    }
  `,
  underline: css`
    text-decoration: underline;
    margin-left: 5px;
  `,
  clearInput: css`
    position: absolute;
    right: 9px;
    top: 50%;
    transform: translateY(-50%);
    cursor: pointer;
    font-size: 18px;
    color: ${Palette.shade3};
  `,
}

const typescriptBaseUrl = 'https://www.typescriptlang.org/play'

interface CreateResponse {
  shortened: string
}

const createLink = async (
  setInputValue: React.Dispatch<React.SetStateAction<string>>,
  setShortened: React.Dispatch<React.SetStateAction<number | null>>,
  setShortenedCreated: (link: string) => void,
  showToast: ShowToast,
  inputValue: string
) => {
  if (!inputValue) return
  if (inputValue.trim().startsWith(typescriptBaseUrl)) {
    try {
      const { shortened } = await api<CreateResponse>('short', { body: { url: inputValue } })
      setShortenedCreated(shortened)
      toast('🔗 Your link was created successfully')
      showToast(
        <span>
          <span role="img" aria-label="check mark">
            ✅
          </span>{' '}
          <strong css={styles.underline}>{shortened}</strong> copied to clipboard
        </span>
      )
      setInputValue('')
      setShortened(prev => (prev ? prev + 1 : 1))
    } catch (e) {
      // eslint-disable-next-line no-console
      console.log('Error trying to shortener URL', e)
      toast('🛑️ There was an error, please try again')
    }
    return
  }
  toast('⚠️ The input text value is not a typescript playground URL')
  setShortenedCreated('')
}

interface Props {
  setShortened: React.Dispatch<React.SetStateAction<number | null>>
  setShortenedCreated: React.Dispatch<React.SetStateAction<string>>
  showToast: ShowToast
}

const LinkCreator: React.FC<Props> = ({ setShortened, setShortenedCreated, showToast }) => {
  const [inputValue, setInputValue] = React.useState('')

  return (
    <div css={styles.container}>
      <div css={styles.inputContainer}>
        <input type="text" css={styles.input} value={inputValue} onChange={e => setInputValue(e.target.value)} />
        {!!inputValue.length && (
          <span css={styles.clearInput} role="button" onClick={() => setInputValue('')}>
            🅧
          </span>
        )}
      </div>
      <button
        css={styles.button}
        onClick={() => createLink(setInputValue, setShortened, setShortenedCreated, showToast, inputValue)}
      >
        Shorten
      </button>
    </div>
  )
}

export default LinkCreator
