import React from 'react'
import useResizeAware from 'react-resize-aware'
import {Sandbox} from './vendor/playground'
import {PluginUtils} from './vendor/PluginUtils'
const {useState, useEffect, createContext, useCallback} = React

type Model = import('monaco-editor').editor.ITextModel

type ModelMarker = import('monaco-editor').editor.IMarker

export type FlashInfo = (message: string) => void

export type ShowModal = {
  (code: string, subtitle?: string, links?: string[]): void
}

export const PluginContext = createContext({})

type ContainerObject = {
  ref: HTMLDivElement
  width: number
  height: number
}

export type PluginContextProps = {
  code: string
  container: ContainerObject
  sandbox: Sandbox
  model: Model
  flashInfo: FlashInfo
  showModal: ShowModal
  markers: (ModelMarker & {key: string})[]
  setCode(value: string, options?: {format: boolean}): void
  formatCode(): void
  setDebounce(debounce: boolean): void
  utils: PluginUtils
}

type ProviderProps = Pick<PluginContextProps, 'sandbox' | 'container' | 'utils'>

export const Provider: React.FC<ProviderProps> = ({
  sandbox,
  container,
  utils,
  children,
}) => {
  const [model, setModel] = useState<Model>()
  const [code, _setCode] = useState(sandbox.getText())
  const [markers, setMarkers] = useState<ModelMarker[]>([])
  const [debounce, setDebounce] = useState(false)
  const [resizeListener, sizes] = useResizeAware()

  const listenerFn = useCallback(
    (evt): void => {
      setModel({...evt.detail.model})
      _setCode(sandbox.getText())
    },
    [sandbox],
  )

  useEffect(() => {
    const disposable = sandbox.editor.onDidChangeModelDecorations(() => {
      const allMarkers = sandbox.monaco.editor
        .getModelMarkers({})
        .map((marker, index) => {
          return {
            ...marker,
            key: index.toString(),
          }
        })
      setMarkers(allMarkers)
    })
    return () => disposable.dispose()
  }, [sandbox])

  useEffect(() => {
    const eventName = debounce ? 'modelChangedDebounce' : 'modelChanged'
    window.addEventListener(eventName, listenerFn)
    const otherEventName = debounce ? 'modelChanged' : 'modelChangedDebounce'
    window.removeEventListener(otherEventName, listenerFn, false)
    return () => window.removeEventListener(eventName, listenerFn, false)
  }, [debounce, listenerFn])

  const setCode = useCallback(
    (value: string, options?: {format: true}) => {
      if (options && options.format) {
        sandbox.setText(value)
        sandbox.editor.getAction('editor.action.formatDocument').run()
      } else {
        sandbox.setText(value)
      }
    },
    [sandbox],
  )

  const formatCode = useCallback(() => {
    return sandbox.editor.getAction('editor.action.formatDocument').run()
  }, [sandbox.editor])

  const {showModal, flashInfo} = window.playground.ui

  const containerWithDimensions: ContainerObject = {
    ref: container,
    ...sizes,
  }

  const value = {
    model,
    showModal,
    flashInfo,
    sandbox,
    container: containerWithDimensions,
    code,
    setCode,
    formatCode,
    setDebounce,
    markers,
    utils,
  }
  return (
    <PluginContext.Provider value={value}>
      {resizeListener}
      {children}
    </PluginContext.Provider>
  )
}
