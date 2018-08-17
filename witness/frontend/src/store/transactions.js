import blockexplorer from '@/api/blockexplorer'

const state = {
  transactions: [],
  perPage: 5,
  nextTransactionHash: undefined
}

const actions = {
  getAllTransactions ({commit}, from = state.nextTransactionHash, perPage = state.perPage + 1) {
    blockexplorer.getTransactions(Transactions => {
      const nextTransactionHash = Transactions.length === (state.perPage + 1) ? Transactions.pop().txId : undefined
      commit('setNextTransactionHash', nextTransactionHash)
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
