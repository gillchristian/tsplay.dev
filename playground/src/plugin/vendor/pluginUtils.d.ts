import {Node} from 'typescript'
/** Creates a set of util functions which is exposed to Plugins to make it easier to build consistent UIs */
export declare const createUtils: (
  sb: any,
) => {
  /** Use this to make a few dumb element generation funcs */
  el: (str: string, el: string, container: Element) => void
  /** Get a relative URL for something in your dist folder depending on if you're in dev mode or not */
  requireURL: (path: string) => string
  /** Returns a div which has an interactive AST a TypeScript AST by passing in the root node */
  createASTTree: (node: Node) => HTMLDivElement
}
export declare type PluginUtils = ReturnType<typeof createUtils>
