import * as worker from "monaco-editor/esm/vs/editor/editor.worker"
// import type * as monaco from "monaco-editor-core"
import { createLanguageService } from "@volar/monaco/worker"
import createTypeScriptService, { createJsDelivrDtsHost } from 'volar-service-typescript';

globalThis.module = {} as any

self.onmessage = (msg) => {
  console.log("got msg", msg)
  if (msg.data.type === "init") {
    // Grab the same version of typescript as the rest of the playground
    const tsURI = `https://unpkg.com/typescript@${msg.data.tsVersion}/lib/typescript.js`
    importScripts(tsURI)
    console.log("grabbed ts")
    console.log({ worker})
    worker.initialize((ctx: any) => {
        console.log("booting worker")
      return createLanguageService({
        workerContext: ctx,
        config: {
          services: {
            typescript: createTypeScriptService({
              // Enable auto fetch node_modules types
              dtsHost: createJsDelivrDtsHost({ typescript:msg.data.tsVersion }),
            }),
          },
        },
        typescript: {
          module: globalThis.ts,
          compilerOptions: {} as any,
        },
      })
    })
  }
}
