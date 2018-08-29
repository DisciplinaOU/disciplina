'use strict'
const merge = require('webpack-merge')
const devEnv = require('./dev.env')

module.exports = merge(devEnv, {
  NODE_ENV: '"testing"',
  WITNESS_API_URL: '"https://witness-1.disciplina.serokell.team"'
})
