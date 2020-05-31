import { apiBaseUrl } from '../constants'

type Options = Omit<RequestInit, 'body'> & { body?: unknown }

const api = <Data = unknown>(endpointUrl: string, { body, ...customConfig }: Options = {}): Promise<Data> => {
  const isPost = Boolean(body)
  const headers = isPost ? { 'Content-Type': 'application/json' } : undefined

  const config = {
    method: isPost ? 'POST' : 'GET',
    ...customConfig,
    headers: {
      ...headers,
      ...customConfig.headers,
    },
  }

  let stringifiedBody: string | undefined

  if (isPost) {
    stringifiedBody = JSON.stringify(body)
  }

  return window
    .fetch(`${apiBaseUrl}/${endpointUrl}`, { ...config, body: stringifiedBody })
    .then(response => (response.ok ? response.json() : Promise.reject(response.json())))
}

export default api
