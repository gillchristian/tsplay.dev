import React from 'react'
import { ToastContainer } from 'react-toastify'
import styled from '@emotion/styled'
import { Palette } from '../constants'

function GlobalToast() {
  return (
    <ToastContainerStyles>
      <ToastContainer pauseOnHover closeButton={false} autoClose={5000} closeOnClick />
    </ToastContainerStyles>
  )
}

const ToastContainerStyles = styled.div`
  .Toastify__toast {
    font-size: 14px;
    box-shadow: 0px 2px 4px -1px rgba(0, 0, 0, 0.2), 0px 4px 5px 0px rgba(0, 0, 0, 0.14),
      0px 1px 10px 0px rgba(0, 0, 0, 0.12);
    background: ${Palette.secondary};
    color: ${Palette.white};

    .Toastify__close-button--default {
      color: ${Palette.white};
      opacity: 0.8;
    }

    .Toastify__progress-bar--default {
      background: ${Palette.primary};
    }
  }
`

export default GlobalToast
