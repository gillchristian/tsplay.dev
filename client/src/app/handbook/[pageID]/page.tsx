import Examples  from "../examples.mdx";
import BugWorkbench from "../bug-workbench.mdx"
import CompilerSettings from "../compiler-settings.mdx"
import ExportingYourCode from "../exporting-your-code.mdx"
import ExtendedEdition from "../extended-edition.mdx"
import GistDocsets from "../gist-docsets.mdx"
import ImplementationDetails from "../implementation-details.mdx"
import JSDTSSidebars from "../js---dts-sidebars.mdx"
import Localization from "../localization.mdx"
import Overview from "../overview.mdx"
import Plugins from "../plugins.mdx"
import RunningCode from "../running-code.mdx"
import SettingsPanel from "../settings-panel.mdx"
import TwoslashAnnotations from "../twoslash-annotations.mdx"
import TypeAcquisition from "../type-acquisition.mdx"
import TypeScriptVersions from "../typescript-versions.mdx"
import URLStructure from "../url-structure.mdx"
import WritingDTSFiles from "../writing-dts-files.mdx"
import WritingJavaScript from "../writing-javascript.mdx"
import WritingPlugins from "../writing-plugins.mdx"


const Page = ({ params }) => {
    const pageID = params.pageID.replace(".html", "")
    switch (pageID) {
      case 'examples': return <Examples />
      case 'bug-workbench': return  <BugWorkbench />
      case 'compiler-settings': return  <CompilerSettings />
      case 'exporting-your-code': return  <ExportingYourCode />
      case 'extended-edition': return  <ExtendedEdition />
      case 'gist-docsets': return  <GistDocsets />
      case 'implementation-details': return  <ImplementationDetails />
      case 'js---dts-sidebars': return  <JSDTSSidebars />
      case 'localization': return  <Localization />
      // case 'multi-file-playgrounds': return  <MultiFilePlaygrounds />
      case 'overview': return  <Overview />
      case 'plugins': return  <Plugins />
      case 'running-code': return  <RunningCode />
      case 'settings-panel': return  <SettingsPanel />
      case 'twoslash-annotations': return  <TwoslashAnnotations />
      case 'type-acquisition': return  <TypeAcquisition />
      case 'typescript-versions': return  <TypeScriptVersions />
      case 'url-structure': return  <URLStructure />
      case 'writing-dts-files': return  <WritingDTSFiles />
      case 'writing-javascript': return  <WritingJavaScript />
      case 'writing-plugins': return  <WritingPlugins />
    }
  }
  
  export default Page