/* 
This script is responsible for opening the TypeScript Playground in Chrome on the first 
build when running the "start" script. It also provides some useful console messages.  

It is called by rollup-plugin-execute in rollup.config.js
*/
const exec = require("child_process").exec;
const getChromeTabs = require("get-chrome-tabs");
const chalk = require("chalk");

const PLAYGROUND_URL = "https://www.typescriptlang.org/v2/en/play";

function openPlayground() {
  exec(`open-cli ${PLAYGROUND_URL} -- 'google chrome'`, function(err) {
    if (err) {
      console.log(
        chalk.red("Error opening the TypeScript Playground. Please try again.")
      );
    } else {
      const message = chalk.green(
        '\n🚀  The TypeScript Playground was opened in Chrome. To view your plugin, select "Options > Connect to localhost:5000/index.js" in the Playground sidebar and refresh the browser tab.'
      );
      console.log(message);
    }
  });
}

let tabList = [];

getChromeTabs()
  .then(tabs => {
    tabList = tabs;
  })
  .catch(err => {
    return err;
  })
  .finally(() => {
    const isPlaygroundOpen = tabList.find(tab =>
      tab.url.includes(PLAYGROUND_URL)
    );
    if (!isPlaygroundOpen) {
      openPlayground();
    } else {
      console.log(
        chalk.green(
          "\n🚀  Your plugin has been updated. Please refresh the TypeScript Playground Chrome tab to see your changes."
        )
      );
    }
  });
