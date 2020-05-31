import * as React from 'react'
import { toast, ToastContent, ToastOptions } from 'react-toastify'

export type ShowToast = (content: ToastContent, options?: ToastOptions, isError?: boolean) => void

export const useCopyClipboardToast = (): ShowToast => {
  const [toastId, setToastId] = React.useState<string | number>('')

  const showToast = React.useCallback<ShowToast>(
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

  return showToast
}
