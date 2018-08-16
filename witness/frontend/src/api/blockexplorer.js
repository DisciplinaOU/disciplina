import Vue from 'vue'

export default {
  getBlocks (cb, page, perPage) {
    // const from = ((page - 1) * perPage)
    // const to = (page * perPage) || 1
    Vue.axios
      .get('/blocks')
      .then(response => cb(response.data))
  },
  getBlock (cb, blockHash) {
    Vue.axios
      .get(`/blocks/${blockHash}`)
      .then(response => cb(response.data))
  },
  getTransactions (cb, page, perPage) {
    // const from = ((page - 1) * perPage)
    // const to = (page * perPage) || 1
    Vue.axios
      .get('/transactions')
      .then(response => cb(response.data.transactions))
  },
  getTransaction (cb, transactionHash) {
    Vue.axios
      .get(`/transactions/${transactionHash}`)
      .then(response => cb(response.data))
  },
  getAddress (cb, addressHash) {
    Vue.axios
      .get(`/address/${addressHash}`)
      .then(response => cb(response.data))
  }
}
