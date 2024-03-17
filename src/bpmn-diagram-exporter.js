const core = require('@actions/core')
const { convertAll } = require('bpmn-to-image')
const fs = require('fs')
const source = 'src/main/resources/bpmn'
const target = 'build/resources/main/bpmn'

async function exportBpmnDiagrams() {
  try {
    const sourceDirectory = core.getInput('sourceDirectory', { required: true })
    const targetDirectory = core.getInput('targetDirectory', { required: true })
    for (const bpmn of fs.readdirSync(sourceDirectory)) {
      core.info(
        `Exporting BPMN at ${sourceDirectory}/${bpmn} to ${targetDirectory}`
      )
      exportBpmnDiagram(sourceDirectory, targetDirectory, bpmn)
      core.info(
        `Successfully exported BPMN at ${sourceDirectory}/${bpmn} to ${targetDirectory}`
      )
    }
  } catch (error) {
    core.setFailed(error.message)
  }
}

async function exportBpmnDiagram(sourceDirectory, targetDirectory, bpmn) {
  convertAll([
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
