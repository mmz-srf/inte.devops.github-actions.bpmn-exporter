const core = require('@actions/core')
const { convertAll } = require('bpmn-to-image')
const fs = require('fs')
// const downloadBrowser = require('puppeteer/install.mjs')

async function exportBpmnDiagrams() {
  try {
    // core.info('Downloading puppeteer browser..')
    // downloadBrowser()
    // core.info('Successfully downloaded puppeteer browser')
    const sourceDirectory = core.getInput('sourceDirectory', { required: true })
    const targetDirectory = core.getInput('targetDirectory', { required: true })
    for (const bpmn of fs.readdirSync(sourceDirectory)) {
      core.info(
        `Exporting BPMN at ${sourceDirectory}/${bpmn} to ${targetDirectory}..`
      )
      await exportBpmnDiagram(sourceDirectory, targetDirectory, bpmn)
      core.info(
        `Successfully exported BPMN at ${sourceDirectory}/${bpmn} to ${targetDirectory}`
      )
    }
  } catch (error) {
    core.setFailed(`Failed to export BPMN: ${error.message}`)
  }
}

async function exportBpmnDiagram(sourceDirectory, targetDirectory, bpmn) {
  await convertAll([
    {
      input: `${sourceDirectory}/${bpmn}`,
      outputs: [`${targetDirectory}/${basename(bpmn)}.svg`]
    }
  ])
}

function basename(filename) {
  return filename.substring(0, filename.lastIndexOf('.'))
}

module.exports = {
  exportBpmnDiagrams
}
