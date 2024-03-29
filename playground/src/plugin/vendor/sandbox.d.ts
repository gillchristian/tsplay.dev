import {TypeScriptWorker} from './tsWorker' // import { TypeScriptWorker } from './tsWorker';
// import lzstring from './vendor/lzstring.min';

import * as tsvfs from './typescript-vfs'
declare type CompilerOptions =
  import('monaco-editor').languages.typescript.CompilerOptions
/**
 * These are settings for the playground which are the equivalent to props in React
 * any changes to it should require a new setup of the playground
 */
export declare type PlaygroundConfig = {
  /** The default source code for the playground */
  text: string
  /** Should it run the ts or js IDE services */
  useJavaScript: boolean
  /** Compiler options which are automatically just forwarded on */
  compilerOptions: CompilerOptions
  /** Optional monaco settings overrides */
  monacoSettings?: import('monaco-editor').editor.IEditorOptions
  /** Acquire types via type acquisition */
  acquireTypes: boolean
  /** Support twoslash compiler options */
  supportTwoslashCompilerOptions: boolean
  /** Get the text via query params and local storage, useful when the editor is the main experience */
  suppressAutomaticallyGettingDefaultText?: true
  /** Suppress setting compiler options from the compiler flags from query params */
  suppressAutomaticallyGettingCompilerFlags?: true
  /** Logging system */
  logger: {
    log: (...args: any[]) => void
    error: (...args: any[]) => void
  }
} & (
  | {
      domID: string
    }
  | {
      elementToAppend: HTMLElement
    }
)
/** The default settings which we apply a partial over */
export declare function defaultPlaygroundSettings(): {
  /** The default source code for the playground */
  text: string
  /** Should it run the ts or js IDE services */
  useJavaScript: boolean
  /** Compiler options which are automatically just forwarded on */
  compilerOptions: import('monaco-editor').languages.typescript.CompilerOptions
  /** Optional monaco settings overrides */
  monacoSettings?: import('monaco-editor').editor.IEditorOptions | undefined
  /** Acquire types via type acquisition */
  acquireTypes: boolean
  /** Support twoslash compiler options */
  supportTwoslashCompilerOptions: boolean
  /** Get the text via query params and local storage, useful when the editor is the main experience */
  suppressAutomaticallyGettingDefaultText?: true | undefined
  /** Suppress setting compiler options from the compiler flags from query params */
  suppressAutomaticallyGettingCompilerFlags?: true | undefined
  /** Logging system */
  logger: {
    log: (...args: any[]) => void
    error: (...args: any[]) => void
  }
} & {
  domID: string
}
/** Creates a sandbox editor, and returns a set of useful functions and the editor */
export declare const createTypeScriptSandbox: (
  partialConfig:
    | Partial<
        {
          /** The default source code for the playground */
          text: string
          /** Should it run the ts or js IDE services */
          useJavaScript: boolean
          /** Compiler options which are automatically just forwarded on */
          compilerOptions: import('monaco-editor').languages.typescript.CompilerOptions
          /** Optional monaco settings overrides */
          monacoSettings?:
            | import('monaco-editor').editor.IEditorOptions
            | undefined
          /** Acquire types via type acquisition */
          acquireTypes: boolean
          /** Support twoslash compiler options */
          supportTwoslashCompilerOptions: boolean
          /** Get the text via query params and local storage, useful when the editor is the main experience */
          suppressAutomaticallyGettingDefaultText?: true | undefined
          /** Suppress setting compiler options from the compiler flags from query params */
          suppressAutomaticallyGettingCompilerFlags?: true | undefined
          /** Logging system */
          logger: {
            log: (...args: any[]) => void
            error: (...args: any[]) => void
          }
        } & {
          domID: string
        }
      >
    | Partial<
        {
          /** The default source code for the playground */
          text: string
          /** Should it run the ts or js IDE services */
          useJavaScript: boolean
          /** Compiler options which are automatically just forwarded on */
          compilerOptions: import('monaco-editor').languages.typescript.CompilerOptions
          /** Optional monaco settings overrides */
          monacoSettings?:
            | import('monaco-editor').editor.IEditorOptions
            | undefined
          /** Acquire types via type acquisition */
          acquireTypes: boolean
          /** Support twoslash compiler options */
          supportTwoslashCompilerOptions: boolean
          /** Get the text via query params and local storage, useful when the editor is the main experience */
          suppressAutomaticallyGettingDefaultText?: true | undefined
          /** Suppress setting compiler options from the compiler flags from query params */
          suppressAutomaticallyGettingCompilerFlags?: true | undefined
          /** Logging system */
          logger: {
            log: (...args: any[]) => void
            error: (...args: any[]) => void
          }
        } & {
          elementToAppend: HTMLElement
        }
      >,
  monaco: typeof import('monaco-editor'),
  ts: typeof import('typescript'),
) => {
  /** The same config you passed in */
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
  /** A list of TypeScript versions you can use with the TypeScript sandbox */
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
  /** The monaco editor instance */
  editor: import('monaco-editor').editor.IStandaloneCodeEditor
  /** Either "typescript" or "javascript" depending on your config */
  language: string
  /** The outer monaco module, the result of require("monaco-editor")  */
  monaco: typeof import('monaco-editor')
  /** Gets a monaco-typescript worker, this will give you access to a language server. Note: prefer this for language server work because it happens on a webworker . */
  getWorkerProcess: () => Promise<TypeScriptWorker>
  /** A copy of require("typescript-vfs") this can be used to quickly set up an in-memory compiler runs for ASTs, or to get complex language server results (anything above has to be serialized when passed)*/
  tsvfs: typeof tsvfs
  /** Get all the different emitted files after TypeScript is run */
  getEmitResult: () => Promise<import('typescript').EmitOutput>
  /** Gets just the JavaScript for your sandbox, will transpile if in TS only */
  getRunnableJS: () => Promise<string>
  /** Gets the DTS output of the main code in the editor */
  getDTSForCode: () => Promise<string>
  /** The monaco-editor dom node, used for showing/hiding the editor */
  getDomNode: () => HTMLElement
  /** The model is an object which monaco uses to keep track of text in the editor. Use this to directly modify the text in the editor */
  getModel: () => import('monaco-editor').editor.ITextModel
  /** Gets the text of the main model, which is the text in the editor */
  getText: () => string
  /** Shortcut for setting the model's text content which would update the editor */
  setText: (text: string) => void
  /** WIP: Gets the AST of the current text */
  getAST: () => Promise<import('typescript').SourceFile>
  /** The module you get from  require("typescript") */
  ts: typeof import('typescript')
  /** Create a new Program, a TypeScript data model which represents the entire project.
   *
   * The first time this is called it has to download all the DTS files which is needed for an exact compiler run. Which
   * at max is about 1.5MB - after that subsequent downloads of dts lib files come from localStorage.
   *
   * You probably want
   */
  createTSProgram: () => Promise<import('typescript').Program>
  /** The Sandbox's default compiler options  */
  compilerDefaults: import('monaco-editor').languages.typescript.CompilerOptions
  /** The Sandbox's current compiler options */
  getCompilerOptions: () => import('monaco-editor').languages.typescript.CompilerOptions
  /** Replace the Sandbox's compiler options */
  setCompilerSettings: (
    opts: import('monaco-editor').languages.typescript.CompilerOptions,
  ) => void
  /** Overwrite the Sandbox's compiler options */
  updateCompilerSetting: (key: string | number, value: any) => void
  /** Update a single compiler option in the SAndbox */
  updateCompilerSettings: (
    opts: import('monaco-editor').languages.typescript.CompilerOptions,
  ) => void
  /** A way to get callbacks when compiler settings have changed */
  setDidUpdateCompilerSettings: (
    func: (
      opts: import('monaco-editor').languages.typescript.CompilerOptions,
    ) => void,
  ) => void
  /** A copy of lzstring, which is used to archive/unarchive code */
  // lzstring: typeof lzstring;
  /** Returns compiler options found in the params of the current page */
  getURLQueryWithCompilerOptions: (sandbox: any, paramOverrides?: any) => string
  /** Returns compiler options in the source code using twoslash notation */
  getTwoSlashComplierOptions: (code: string) => any
  /** Gets to the current monaco-language, this is how you talk to the background webworkers */
  languageServiceDefaults: import('monaco-editor').languages.typescript.LanguageServiceDefaults
}
export declare type Sandbox = ReturnType<typeof createTypeScriptSandbox>
export {}
