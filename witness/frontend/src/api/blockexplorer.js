import Vue from 'vue'

export default {
  getBlocks (cb, skip, count) {
    Vue.axios
      .get('/blocks', { params: {skip, count} })
      .then(response => cb(response.data.totalCount, response.data.blocks))
  },
  getBlock (cb, blockHash) {
    Vue.axios
      .get(`/blocks/${blockHash}`)
      .then(response => cb(response.data))
  },
  getTransactions (cb, from, count) {
    Vue.axios
      .get('/transactions', { params: {count, from} })
      .then(response => cb(response.data.nextId, response.data.transactions))
  },
  getTransaction (cb, transactionHash) {
    Vue.axios
      .get(`/transactions/${transactionHash}`)
      .then(response => cb(response.data))
  },
  getAddress (cb, addressHash) {
    Vue.axios
      .get(`/accounts/${addressHash}`, {params: { includeTxs: true }})
      .then(response => cb(response.data))
  },
  getType (cb, hash) {
    Vue.axios
      .get(`/hash/${hash}`)
      .then(response => cb(response.data))
  }
}
