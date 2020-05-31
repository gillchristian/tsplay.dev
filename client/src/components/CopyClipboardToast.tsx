import * as React from 'react'
import { toast, ToastContent, ToastOptions } from 'react-toastify'

export type ShowToastFcType = (content: ToastContent, options?: ToastOptions, isError?: Boolean) => void

interface ITProps {
  children: (data: { showToast: ShowToastFcType }) => JSX.Element | null
}

const CopyClipboardToast: React.FC<ITProps> = props => {
  const [toastId, setToastId] = React.useState<React.ReactText>('')

  const showToast = React.useCallback<ShowToastFcType>(
    (content, options = {}, isError) => {
      const toastFc = isError ? toast.error : toast
      if (toastId) {
        toast.dismiss(toastId)
        options.delay = 270
      }
      const newToastId = toastFc(content, options)
      setToastId(newToastId)
    },
    [toastId]
  )

  return props.children({ showToast })
}

export default CopyClipboardToast
