import { PlaygroundPlugin, PluginFactory } from ".."

export const shortenerPlugin: PluginFactory = (i, utils) => {
  let codeElement: HTMLElement

  const plugin: PlaygroundPlugin = {
    id: "shortener",
    displayName: i("play_sidebar_shortener"),
    willMount: (_, container) => {
      const { code } = utils.createDesignSystem(container)
      codeElement = code("Here we will add the link shortener")
    },
    modelChangedDebounce: (sandbox, model) => {
      sandbox.getRunnableJS().then((js) => {
        sandbox.monaco.editor.colorize(js, "javascript", {}).then((coloredJS) => {
          console.log(js)
          console.log(coloredJS)
        })
      })
    },
  }

  return plugin
}
