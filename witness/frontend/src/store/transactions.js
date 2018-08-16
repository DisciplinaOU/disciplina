import blockexplorer from '@/api/blockexplorer'

const state = {
  transactions: [],
  listFinished: false
}

const actions = {
  checkListFinished ({commit}, page = 1, perPage = 1) {
    blockexplorer.getTransactions(Transactions => {
      commit('setListFinished', Transactions)
    }, page + 1, perPage)
  },
  getAllTransactions ({commit}, page = 1, perPage = 1) {
    blockexplorer.getTransactions(Transactions => {
      commit('setTransactions', Transactions)
    }, page, perPage)
  }
}

const mutations = {
  setListFinished (state, transactions) {
    state.listFinished = transactions.length === 0
  },
  setTransactions (state, transactions) {
    state.transactions.push(...transactions)
  }
}

const getters = {
  listFinished (state) {
    return state.listFinished
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
