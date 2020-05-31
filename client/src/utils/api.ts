import { apiBaseUrl } from '../constants'

// TODO: Add correct types
const api = (endpointUrl: string, { body, ...customConfig }: any = {}) => {
  const headers = { 'Content-Type': 'application/json' }
  const config = {
    method: body ? 'POST' : 'GET',
    ...customConfig,
    headers: {
      ...headers,
      ...customConfig.headers,
    },
  }
  if (body) {
    config.body = JSON.stringify(body)
  }
  return window.fetch(`${apiBaseUrl}/${endpointUrl}`, config).then(async response => {
    const data = await response.json()
    if (response.ok) {
      return data
    } else {
      return Promise.reject(data)
    }
  })
}

export default api
