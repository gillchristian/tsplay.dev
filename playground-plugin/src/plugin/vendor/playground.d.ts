declare type Sandbox = import('./sandbox').Sandbox
export {PluginUtils} from './pluginUtils'
export declare type PluginFactory = {
  (i: (key: string, components?: any) => string): PlaygroundPlugin
}
/** The interface of all sidebar plugins */
export interface PlaygroundPlugin {
  /** Not public facing, but used by the playground to uniquely identify plugins */
  id: string
  /** To show in the tabs */
  displayName: string
  /** Should this plugin be selected when the plugin is first loaded? Let's you check for query vars etc to load a particular plugin */
  shouldBeSelected?: () => boolean
  /** Before we show the tab, use this to set up your HTML - it will all be removed by the playground when someone navigates off the tab */
  willMount?: (sandbox: Sandbox, container: HTMLDivElement) => void
  /** After we show the tab */
  didMount?: (sandbox: Sandbox, container: HTMLDivElement) => void
  /** Model changes while this plugin is actively selected  */
  modelChanged?: (
    sandbox: Sandbox,
    model: import('monaco-editor').editor.ITextModel,
  ) => void
  /** Delayed model changes while this plugin is actively selected, useful when you are working with the TS API because it won't run on every keypress */
  modelChangedDebounce?: (
    sandbox: Sandbox,
    model: import('monaco-editor').editor.ITextModel,
  ) => void
  /** Before we remove the tab */
  willUnmount?: (sandbox: Sandbox, container: HTMLDivElement) => void
  /** After we remove the tab */
  didUnmount?: (sandbox: Sandbox, container: HTMLDivElement) => void
  /** An object you can use to keep data around in the scope of your plugin object */
  data?: any
}
interface PlaygroundConfig {
  lang: string
  prefix: string
}

export type Sandbox = {
  config: {
    text: string
    useJavaScript: boolean
    compilerOptions: import('monaco-editor').languages.typescript.CompilerOptions
    monacoSettings?: import('monaco-editor').editor.IEditorOptions | undefined
    acquireTypes: boolean
    supportTwoslashCompilerOptions: boolean
    suppressAutomaticallyGettingDefaultText?: true | undefined
    suppressAutomaticallyGettingCompilerFlags?: true | undefined
    logger: {
      log: (...args: any[]) => void
      error: (...args: any[]) => void
    }
    domID: string
  }
  supportedVersions: readonly [
    '2.4.1',
    '2.7.2',
    '2.8.1',
    '3.0.1',
    '3.1.6',
    '3.3.3',
    '3.5.1',
    '3.6.3',
    '3.7.5',
    '3.8.2',
  ]
  editor: import('monaco-editor').editor.IStandaloneCodeEditor
  language: string
  monaco: typeof import('monaco-editor')
  // getWorkerProcess: () => Promise<import("typescriptlang-org/static/js/sandbox/tsWorker").TypeScriptWorker>;
  tsvfs: typeof import('./typescript-vfs')
  getEmitResult: () => Promise<import('typescript').EmitOutput>
  getRunnableJS: () => Promise<string>
  getDTSForCode: () => Promise<string>
  getDomNode: () => HTMLElement
  getModel: () => import('monaco-editor').editor.ITextModel
  getText: () => string
  setText: (text: string) => void
  getAST: () => Promise<import('typescript').SourceFile>
  ts: typeof import('typescript')
  createTSProgram: () => Promise<import('typescript').Program>
  compilerDefaults: import('monaco-editor').languages.typescript.CompilerOptions
  getCompilerOptions: () => import('monaco-editor').languages.typescript.CompilerOptions
  setCompilerSettings: (
    opts: import('monaco-editor').languages.typescript.CompilerOptions,
  ) => void
  updateCompilerSetting: (key: string | number, value: any) => void
  updateCompilerSettings: (
    opts: import('monaco-editor').languages.typescript.CompilerOptions,
  ) => void
  setDidUpdateCompilerSettings: (
    func: (
      opts: import('monaco-editor').languages.typescript.CompilerOptions,
    ) => void,
  ) => void
  // lzstring: typeof import("typescriptlang-org/static/js/sandbox/vendor/lzstring.min");
  getURLQueryWithCompilerOptions: (sandbox: any, paramOverrides?: any) => string
  getTwoSlashComplierOptions: (code: string) => any
  languageServiceDefaults: import('monaco-editor').languages.typescript.LanguageServiceDefaults
}

export declare const setupPlayground: (
  sandbox: Sandbox,
  monaco: typeof import('monaco-editor'),
  config: PlaygroundConfig,
  i: (key: string) => string,
) => {
  exporter: {
    openProjectInStackBlitz: () => void
    openProjectInCodeSandbox: () => void
    reportIssue: () => Promise<void>
    copyAsMarkdownIssue: () => Promise<void>
    copyForChat: () => void
    copyForChatWithPreview: () => void
    openInTSAST: () => void
  }
  // ui: import("./createUI").UI;
  registerPlugin: (plugin: PlaygroundPlugin) => void
}
export declare type Playground = ReturnType<typeof setupPlayground>
