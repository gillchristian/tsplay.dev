import * as worker from "monaco-editor/esm/vs/editor/editor.worker"
// import type * as monaco from "monaco-editor-core"
import { createJsDelivrFs, createJsDelivrUriResolver, decorateServiceEnvironment } from "@volar/cdn"
import { createLanguageHost, createLanguageService, createServiceEnvironment } from "@volar/monaco/worker"
import createTypeScriptService from "volar-service-typescript"

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
      
      const env = createServiceEnvironment();

      // Enable auto fetch node_modules types
      decorateServiceEnvironment(env, createJsDelivrUriResolver("/node_modules", { typescript: msg.data.tsVersion }), createJsDelivrFs());

      return createLanguageService(
        { typescript: globalThis.ts },
        env,
        {
          services: {
            typescript: createTypeScriptService(),
          },
        },
        createLanguageHost(ctx.getMirrorModels, env, "/", {}))
    })
  }
}
