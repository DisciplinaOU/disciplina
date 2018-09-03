import blockexplorer from '@/api/blockexplorer'

const state = {
  transactions: [],
  perPage: 5,
  nextTransactionHash: undefined
}

const actions = {
  getAllTransactions ({commit}, from = state.nextTransactionHash, perPage = state.perPage) {
    blockexplorer.getTransactions((nextId, Transactions) => {
      if (state.nextTransactionHash === undefined) {
        setAutoupdateList(this)
      }
      commit('setNextTransactionHash', nextId)
      commit('setTransactions', Transactions)
    }, from, perPage)
  },
  fetchNewTransactions ({commit}, from) {
    blockexplorer.getTransactions((nextId, Transactions) => {
      for (var i = Transactions.length - 1; i >= 0; i--) {
        if (Transactions[i].txId === state.transactions[0].txId) {
          commit('unshiftTransactions', Transactions.slice(0, i))
          break
        }
      }
    }, undefined, state.perPage)
  }
}

const mutations = {
  setNextTransactionHash (state, hash) {
    state.nextTransactionHash = hash
  },
  setTransactions (state, transactions) {
    state.transactions.push(...transactions)
  },
  unshiftTransactions (state, transactions) {
    state.transactions.unshift(...transactions)
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

function setAutoupdateList (store) {
  setInterval(() => {
    store.dispatch('fetchNewTransactions')
  }, 20000)
}

export default {
  state,
  actions,
  mutations,
  getters
}
