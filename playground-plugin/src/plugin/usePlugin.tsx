import React from 'react'
import {PluginContext, PluginContextProps} from './Provider'
const {useContext} = React

export function usePlugin() {
  return useContext(PluginContext) as PluginContextProps
}
