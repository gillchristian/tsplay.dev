"use client"

import * as sandbox from "./sandbox/src/index"
import playgroundReleases from "./sandbox/src/releases.json"
import * as playground from "./playground/src/index"

import playgroundHandbookTOC from "./vendor/play-handbook.json"
import optionsSummary from "./vendor/en-summary.json"
import examples from "./examples/metadata.json"

import React, { useEffect } from "react"
import ReactDOM from "react-dom"

const i = (s: string) => (playCopy as any)[s] || s

import "./playground.scss"
import { playCopy } from "./en"
import { RenderExamples } from "./components/ShowExamples"
export function Playground() {
  useEffect(() => {
    // Don't even bother getting monaco

    // @ts-ignore - so the playground handbook can grab this data
    window.playgroundHandbookTOC  =playgroundHandbookTOC
    // @ts-ignore - so the config options can use localized descriptions
    window.optionsSummary = optionsSummary.options
    // @ts-ignore - for React-based plugins
    window.react = React
    // @ts-ignore - for React-based plugins
    window.reactDOM = ReactDOM
    // @ts-ignore - so that plugins etc can use i18n
    window.i = () => ""

    const getLoaderScript = document.createElement("script")
    getLoaderScript.src = "/js/vs.loader.js"
    getLoaderScript.async = true
    getLoaderScript.onload = async () => {
      // @ts-ignore
      const re: any = global.require
      const params = new URLSearchParams(location.search)

      let tsVersionParam = params.get("ts")
      // handle the nightly lookup
      if ((tsVersionParam && tsVersionParam === "Nightly") || tsVersionParam === "next") {
        // Avoids the CDN to doubly skip caching
        const nightlyLookup = await fetch("https://tswebinfra.blob.core.windows.net/indexes/next.json", { cache: "no-cache" })
        const nightlyJSON = await nightlyLookup.json()
        tsVersionParam = nightlyJSON.version
      }

      // Somehow people keep trying -insiders urls instead of -dev - maybe some tooling I don't know?
      if (tsVersionParam && tsVersionParam.includes("-insiders.")) {
        tsVersionParam = tsVersionParam.replace("-insiders.", "-dev.")
      }

      const latestRelease = [...playgroundReleases.versions].sort().pop()!
      const tsVersion = tsVersionParam || latestRelease

      // Because we can reach to localhost ports from the site, it's possible for the locally built compiler to
      // be hosted and to power the editor with a bit of elbow grease.
      const useLocalCompiler = tsVersion === "dev"
      const devIsh = ["pr", "dev"]
      const version = devIsh.find((d) => tsVersion.includes(d)) ? "dev" : "min"
      const urlForMonaco = useLocalCompiler
        ? "http://localhost:5615/dev/vs"
        : `https://typescript.azureedge.net/cdn/${tsVersion}/monaco/${version}/vs`

      // Make a quick HEAD call for the main monaco editor for this version of TS, if it
      // bails then give a useful error message and bail.
      const nightlyLookup = await fetch(urlForMonaco + "/editor/editor.main.js", { method: "HEAD" })
      if (!nightlyLookup.ok) {
        document.querySelectorAll<HTMLDivElement>(".lds-grid div").forEach((div) => {
          div.style.backgroundColor = "red"
          div.style.animation = ""
          div.style.webkitAnimation = ""
        })

        // prettier-ignore
        document.getElementById("loading-message")!.innerHTML = `This version of TypeScript <em>(${tsVersion?.replace(/</g,"-")})</em><br/>has not been prepared for the Playground<br/><br/>Try <a href='/play?ts=${latestRelease}${document.location.hash}'>${latestRelease}</a> or <a href="/play?ts=next${document.location.hash}">Nightly</a>`
        return
      }

      re.config({
        paths: {
          vs: urlForMonaco,
          unpkg: "https://unpkg.com",
          local: "http://localhost:5000",
        },
        ignoreDuplicateModules: ["vs/editor/editor.main"],
        catchError: true,
        onError: function (err: any) {
          if (document.getElementById("loading-message")) {
            document.getElementById("loading-message")!.innerText = "Cannot load the Playground in this browser"
            console.error(
              "Error setting up monaco/sandbox/playground from the JS, this is likely that you're using a browser which monaco doesn't support."
            )
          } else {
            console.error("Caught an error which is likely happening during initializing a playground plugin:")
          }
          console.error(err)
        },
      })

      re(
        ["vs/editor/editor.main", "vs/language/typescript/tsWorker"],
        async (main: typeof import("monaco-editor"), tsWorker: typeof import("typescript")) => {
          const ts = (global as any).ts || tsWorker.typescript
          const isOK = main && ts
          if (isOK) {
            document.getElementById("loader")!.parentNode?.removeChild(document.getElementById("loader")!)
          } else {
            console.error("Error setting up all the 4 key dependencies")
            console.error("main", !!main, "ts", !!ts, "sandbox", sandbox, "playground", !!playground)
            document.getElementById("loading-message")!.innerText = "Cannot load the Playground in this browser, see logs in console."
            return
          }

          // Set the height of monaco to be either your window height or 600px - whichever is smallest
          const container = document.getElementById("playground-container")!
          container.style.display = "flex"
          const height = Math.max(window.innerHeight, 600)
          container.style.height = `${height - Math.round(container.getClientRects()[0].top) - 18}px`

          const extension = (!!params.get("useJavaScript") ? "js" : params.get("filetype") || "ts") as any
          const workerPath = undefined //  params.get("multiFile") ? `${document.location.origin + playgroundWorker}?filetype=${extension}` : undefined

          
          // Create the sandbox
          const sandboxEnv = await sandbox.createTypeScriptSandbox(
            {
              text: localStorage.getItem("sandbox-history") || i("play_default_code_sample"),
              compilerOptions: {},
              domID: "monaco-editor-embed",
              filetype: extension,
              acquireTypes: !localStorage.getItem("disable-ata"),
              supportTwoslashCompilerOptions: true,
              customTypeScriptWorkerPath: workerPath,
              monacoSettings: {
                fontFamily: "var(--code-font)",
                fontLigatures: true,
              },
            },
            main,
            ts
          )

          const playgroundConfig = {
            lang: "en",
            prefix: "/",
            supportCustomPlugins: true,
          }
          playground.setupPlayground(sandboxEnv, main, playgroundConfig, i as any, React)

          // Dark mode faff
          const darkModeEnabled = document.documentElement.classList.contains("dark-theme")
          if (darkModeEnabled) {
            sandboxEnv.monaco.editor.setTheme("sandbox-dark")
          }

          sandboxEnv.editor.focus()
          sandboxEnv.editor.layout()
        }
      )
    }

    document.body.appendChild(getLoaderScript)
  }, [])

  return (
    <main>

      {/** This is the top nav, which is outside of the editor  */}
      <nav className="navbar-sub">
        <ul className="nav">
          <li className="name hide-small">
            <span>Playground</span>
          </li>

          <li className="dropdown">
            <a
              id="compiler-options-button"
              href="#"
              className="dropdown-toggle"
              data-toggle="dropdown"
              role="button"
              aria-haspopup="menu"
              aria-expanded="false"
              aria-controls="compiler-options-dropdown"
            >
              {i("play_subnav_config")} <span className="caret"></span>
            </a>
            <div id="compiler-options-dropdown" className="dropdown-dialog" aria-labelledby="compiler-options-button">
              <h3>{i("play_subnav_config")}</h3>
              <div className="info" id="config-container">
                <button className="examples-close">{i("play_subnav_examples_close")}</button>

                <div id="compiler-dropdowns">
                  <label className="select">
                    <span className="select-label">Lang</span>
                    <select id="language-selector">
                      <option>TypeScript</option>
                      <option>TypeScript Definitions</option>
                      <option>JavaScript</option>
                    </select>
                    <span className="compiler-flag-blurb">{i("play_config_language_blurb")}</span>
                  </label>
                </div>
              </div>
            </div>
          </li>

          <li className="dropdown">
            <a
              href="#"
              id="examples-button"
              className="dropdown-toggle"
              data-toggle="dropdown"
              role="button"
              aria-haspopup="menu"
              aria-expanded="false"
              aria-controls="examples"
            >
              {i("play_subnav_examples")} <span className="caret"></span>
            </a>
            <div className="dropdown-dialog" id="examples" aria-labelledby="examples-button">
              <button className="examples-close" aria-label="Close dropdown" role="button">
                {i("play_subnav_examples_close")}
              </button>
               <RenderExamples
                defaultSection="TypeScript"
                sections={["JavaScript", "TypeScript"]}
                examples={examples}
                locale={"en"}
              /> *
            </div>
          </li>

          <li className="dropdown">
            <a
              href="#"
              id="handbook-button"
              className="dropdown-toggle"
              data-toggle="dropdown"
              role="button"
              aria-haspopup="menu"
              aria-expanded="false"
              aria-controls="examples"
            >
              {i("play_subnav_handbook")} <span className="caret"></span>
            </a>
          </li>
        </ul>

        <ul className="nav navbar-nav navbar-right hidden-xs">
          <li>
            <a href="#" id="playground-settings" role="button">
              Settings
            </a>
          </li>
        </ul>
      </nav>

      <div className="raised" style={{ paddingTop: "0", marginTop: "0", marginBottom: "3rem", paddingBottom: "1.5rem" }}>
        <div id="loader">
          <div className="lds-grid">
            <div></div>
            <div></div>
            <div></div>
            <div></div>
            <div></div>
            <div></div>
            <div></div>
            <div></div>
            <div></div>
          </div>
          <p id="loading-message" role="status">
            {i("play_downloading_typescript")}
          </p>
        </div>
        <div id="playground-container" style={{ display: "none" }}>
          <div id="editor-container">
            <div id="story-container" style={{ display: "none" }}></div>
            <div id="editor-toolbar" className="navbar-sub">
              <ul>
                <li id="versions" className="dropdown">
                  <a
                    href="#"
                    data-toggle="dropdown"
                    role="button"
                    aria-haspopup="menu"
                    aria-expanded="false"
                    aria-controls="versions-dropdown"
                    id="versions-button"
                  >
                    {i("play_downloading_version")}... <span className="caret" />
                  </a>
                  <ul className="dropdown-menu versions" id="versions-dropdown" aria-labelledby="versions-button"></ul>
                </li>
                <li>
                  <a id="run-button" href="#" role="button">
                    {i("play_toolbar_run")}
                  </a>
                </li>

                <li className="dropdown">
                  <a
                    href="#"
                    id="exports-dropdown"
                    className="dropdown-toggle"
                    data-toggle="dropdown"
                    role="button"
                    aria-haspopup="true"
                    aria-expanded="false"
                    aria-controls="export-dropdown-menu"
                  >
                    {i("play_toolbar_export")} <span className="caret"></span>
                  </a>
                  <ul className="dropdown-menu" id="export-dropdown-menu" aria-labelledby="whatisnew-button">
                    <li>
                      <a href="#" onClick={() => globalThis.playground.exporter.exportAsTweet()} aria-label={i("play_export_tweet_md")}>
                        {i("play_export_tweet_md")}
                      </a>
                    </li>
                    <li role="separator" className="divider"></li>
                    <li>
                      <a href="#" onClick={(e: any) => globalThis.playground.exporter.copyAsMarkdownIssue(e)} aria-label={i("play_export_copy_md")}>
                        {i("play_export_copy_md")}
                      </a>
                    </li>
                    <li>
                      <a href="#" onClick={(e: any) => playground.exporter.copyForChat(e)} aria-label={i("play_export_copy_link")}>
                        {i("play_export_copy_link")}
                      </a>
                    </li>
                    <li>
                      {" "}
                      <a
                        href="#"
                        onClick={(e: any) => playground.exporter.copyForChatWithPreview(e)}
                        aria-label={i("play_export_copy_link_preview")}
                      >
                        {i("play_export_copy_link_preview")}
                      </a>
                    </li>
                    <li role="separator" className="divider"></li>
                    <li>
                      <a href="#" onClick={() => playground.exporter.openInTSAST()} aria-label={i("play_export_tsast")}>
                        {i("play_export_tsast")}
                      </a>
                    </li>
                    <li>
                      <a href="#" onClick={() => playground.exporter.openInBugWorkbench()} aria-label={i("play_export_bugworkbench")}>
                        {i("play_export_bugworkbench")}
                      </a>
                    </li>
                    <li>
                      <a href="#" onClick={() => playground.exporter.openInVSCodeDev()} aria-label={i("play_export_vscode_dev_play")}>
                        {i("play_export_vscode_dev_play")}
                      </a>
                    </li>
                    <li role="separator" className="divider"></li>
                    <li>
                      <a href="#" onClick={() => playground.exporter.openProjectInCodeSandbox()} aria-label={i("play_export_sandbox")}>
                        {i("play_export_sandbox")}
                      </a>
                    </li>
                    <li>
                      <a href="#" onClick={() => playground.exporter.openProjectInStackBlitz()} aria-label={i("play_export_stackblitz")}>
                        {i("play_export_stackblitz")}
                      </a>
                    </li>
                  </ul>
                </li>
                <li>
                  <a id="share-button" href="#" role="button">
                    {i("play_toolbar_share")}
                  </a>
                </li>
              </ul>

              <ul className="right">
                <li>
                  <a id="sidebar-toggle" aria-label="Hide Sidebar" href="#">
                    &#x21E5;
                  </a>
                </li>
              </ul>
            </div>
            {/** This is the div which monaco is added into - careful, lots of changes happen here at runtime **/}
            <div id="monaco-editor-embed" />
          </div>
        </div>
      </div>
    </main>
  )
}
