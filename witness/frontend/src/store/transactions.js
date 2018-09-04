import blockexplorer from '@/api/blockexplorer'

const state = {
  transactions: [],
  perPage: 5,
  nextTransactionHash: undefined
}

const actions = {
  getAllTransactions ({commit}, from = state.nextTransactionHash, perPage = state.perPage) {
    blockexplorer.getTransactions((nextId, Transactions) => {
      commit('setNextTransactionHash', nextId)
      commit('setTransactions', Transactions)
    }, from, perPage)
  }
}

const mutations = {
  setNextTransactionHash (state, hash) {
    state.nextTransactionHash = hash
  },
  setTransactions (state, transactions) {
    state.transactions.push(...transactions)
  }
}

const getters = {
  nextTransactionHash (state) {
    return state.nextTransactionHash
  },
  transactions (state) {
    return state.transactions
  }
}

export default {
  state,
  actions,
  mutations,
  getters
}
