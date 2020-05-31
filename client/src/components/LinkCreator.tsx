/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import { lighten } from 'polished'
import { Palette } from '../constants'
import { toast } from 'react-toastify'
import api from '../utils/api'

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
  input: css`
    width: 80%;
    height: ${CONTAINER_HEIGHT}px;
    background: ${Palette.shade1};
    outline: none;
    padding: 8px 15px;
    box-sizing: border-box;
    border: none;
    color: ${Palette.secondary};
    font-size: 22px;

    @media (max-width: 550px) {
      width: 90%;
    }
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
}

const typescriptBaseUrl = 'https://www.typescriptlang.org/play'

interface CreateResponse {
  shortened: string;
}

const createLink = async (
  inputRef: HTMLInputElement | null,
  setShortened: React.Dispatch<React.SetStateAction<number | null>>,
  setShortenedCreated: (link: string) => void
) => {
  // TODO use controlled input instead
  if (inputRef && inputRef.value.trim().startsWith(typescriptBaseUrl)) {
    try {
      const url = inputRef.value
      const { shortened } = await api<CreateResponse>('short', { body: { url } })
      setShortenedCreated(shortened)
      toast('🔗 Your link was created successfully')
      inputRef.value = ''
      setShortened(prev => {
        if (typeof prev === 'number') {
          return prev++
        }
        return null
      })
    } catch (e) {
      console.log('Error trying to shortener URL', e)
      toast('🛑️ There was an error, please try again')
    }
    return
  }
  toast('⚠️ The input text value is not a typescript playground URL')
}

interface Props {
  setShortened: React.Dispatch<React.SetStateAction<number | null>>
  setShortenedCreated: React.Dispatch<React.SetStateAction<string>>
}

const LinkCreator: React.FC<Props> = ({ setShortened, setShortenedCreated }) => {
  const inputRef = React.useRef<HTMLInputElement>(null)

  return (
    <div css={styles.container}>
      <input type="text" css={styles.input} ref={inputRef} />
      <button css={styles.button} onClick={() => createLink(inputRef.current, setShortened, setShortenedCreated)}>
        Shorten
      </button>
    </div>
  )
}

export default LinkCreator
