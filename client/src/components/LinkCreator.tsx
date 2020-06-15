/** @jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/core'
import { lighten } from 'polished'
import { Palette } from '../constants'
import { toast } from 'react-toastify'
import api from '../utils/api'
import { ShowToast } from '../hooks/useCopyClipboardToast'
import rollingSvgImg from '../assets/rolling.svg'
import localStorage from '../utils/localStorage'
import { linksLocalStorageKey } from '../constants'
import CustomLinkString from './CustomLinkString'
import ClearInput from './ClearInput'
import { copyToClipboard } from '../utils/copyToClipboard'

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
    padding: 10px 20px;
    padding-right: 30px;
    box-sizing: border-box;
    border: none;
    color: ${Palette.secondary};
    font-size: 22px;
    border-radius: 0;
    margin: 0;
    padding-right: 40px;
    position: relative;
    right: 1px;
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
    cursor: pointer;
    position: relative;
    margin: 0;

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
    right: 14px;
    top: 50%;
    transform: translateY(-50%);
  `,
  rollingSvg: css`
    position: absolute;
    left: 50%;
    top: 50%;
    transform: translate(-50%, -50%);
    width: 33px;
    height: 33px;
  `,
}

const typescriptBaseUrl = 'https://www.typescriptlang.org/play'

const handleLinksList = (url: string, setLinks: (links: string[]) => void): void => {
  try {
    const links: string[] = localStorage.get(linksLocalStorageKey) || []
    // Note: filter repeated links
    const filteredLinks = [url, ...links.filter(link => link !== url)]
    localStorage.set(linksLocalStorageKey, filteredLinks)
    setLinks(filteredLinks)
  } catch (error) {
    // eslint-disable-next-line no-console
    console.log('error saving link in local storage')
    // eslint-disable-next-line no-console
    console.log(error)
  }
}

interface CreateResponse {
  shortened: string
}

interface CreatePayload {
  url: string
  // optional custom short link: tsplay.dev/<short>
  // validation rules:
  //     - 5 <= short.length <= 30
  //     - starts with letters
  //     - ends with letters or numbers
  //     - contains letters, numbers, '-' & '_'
  short?: string
  // for alanytics
  createdOn?: 'client' | 'plugin' | 'api' | 'other'
  expires?: boolean
}

type Validation = [true] | [false, string]

const validateCustomLink = (customLink: string): Validation => {
  const startWithChar = /^[a-zA-Z]$/.test(customLink.charAt(0))
  const endWithCharOrNumber = /^[a-zA-Z0-9]$/.test(customLink.substr(-1))
  const bodyValidation = /^[a-zA-Z][a-zA-Z0-9_-]{3,28}[a-zA-Z0-9]$/.test(customLink)

  if (!startWithChar) {
    return [false, 'Custom link back-half should start with a letter']
  }
  if (!endWithCharOrNumber) {
    return [false, 'Custom link back-half should end with a number or a letter']
  }
  if (!bodyValidation) {
    return [
      false,
      'Custom link back-half should have between 5 and 30 characters and contain only letters, numbers, "-" and "_"',
    ]
  }

  return [true]
}

const useLinkInput = ({ setShortened, setShortenedCreated, showToast, setLinks }: Props) => {
  const [inputValue, setInputValue] = React.useState('')
  const [loading, setLoading] = React.useState(false)
  const [customLink, setCustomLink] = React.useState('')

  const createLink = async () => {
    if (!inputValue) return
    if (!inputValue.trim().startsWith(typescriptBaseUrl)) {
      toast(
        <div>
          <span role="img" aria-label="warning" className="prevent-hue-rotate">
            ⚠️{' '}
          </span>
          Not a TypeScript Playground URL
        </div>
      )
      return
    }
    if (customLink) {
      const [isValid, msg] = validateCustomLink(customLink)
      if (!isValid) {
        toast(msg)
        return
      }
    }
    try {
      setLoading(true)
      const body: CreatePayload = {
        url: inputValue,
        createdOn: 'client',
        expires: false,
        ...(customLink && { short: customLink }),
      }
      const { shortened } = await api<CreateResponse>('short', { body })
      setShortenedCreated(shortened)
      handleLinksList(shortened, setLinks)
      setCustomLink('')
      toast(
        <div>
          <span role="img" aria-label="warning" className="prevent-hue-rotate">
            🔗{' '}
          </span>
          Short link created successfully!
        </div>
      )
      copyToClipboard(shortened.replace(/^https?:\/\//, ''), showToast)
      setInputValue('')
      setShortened(prev => (prev ? prev + 1 : 1))
    } catch (e) {
      // eslint-disable-next-line no-console
      console.log('Error trying to shorten URL', e)
      toast(
        <div>
          <span role="img" aria-label="warning" className="prevent-hue-rotate">
            🛑️{' '}
          </span>
          Opps, something went wrong.
        </div>
      )
    } finally {
      setLoading(false)
    }
  }

  return { inputValue, loading, setInputValue, createLink, customLink, setCustomLink }
}

interface Props {
  setShortened: React.Dispatch<React.SetStateAction<number | null>>
  setShortenedCreated: React.Dispatch<React.SetStateAction<string>>
  showToast: ShowToast
  setLinks: (links: string[]) => void
}

const LinkCreator: React.FC<Props> = props => {
  const { inputValue, loading, setInputValue, createLink, customLink, setCustomLink } = useLinkInput(props)

  return (
    <React.Fragment>
      <div css={styles.container}>
        <div css={styles.inputContainer}>
          <input type="text" css={styles.input} value={inputValue} onChange={e => setInputValue(e.target.value)} />
          {inputValue.length > 0 && (
            <div css={styles.clearInput}>
              <ClearInput onClear={() => setInputValue('')} />
            </div>
          )}
        </div>
        <button css={styles.button} className="prevent-hue-rotate" onClick={createLink}>
          {loading ? <img alt="" src={rollingSvgImg} css={styles.rollingSvg} /> : 'Shorten'}
        </button>
      </div>
      <CustomLinkString customLink={customLink} setCustomLink={setCustomLink} />
    </React.Fragment>
  )
}

export default LinkCreator
