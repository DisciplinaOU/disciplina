import blockexplorer from '@/api/blockexplorer'

const state = {
  transactions: [],
  current: {},
  currentEducator: {}
}

const actions = {
  getAllTransactions ({commit}) {
    blockexplorer.getTransactions(Transactions => {
      commit('setTransactions', Transactions)
    })
  },
  getTransaction ({commit, hash}) {
    blockexplorer.getTransaction(Transaction => {
      commit('setCurrentTransaction', Transaction)
    }, hash)
  },
  getEducatorTransaction ({commit, hash}) {
    blockexplorer.getEducatorTransaction(EducatorTransaction => {
      commit('setCurrentEducatorTransaction', EducatorTransaction)
    }, hash)
  }
}

const mutations = {
  setTransactions (state, transactions) {
    state.all = transactions
  },
  setCurrentTransaction (state, transaction) {
    state.current = transaction
  },
  setCurrentEducatorTransaction (state, educatorTransaction) {
    state.currentEducator = educatorTransaction
  }
}

const getters = {
  transactions (state) {
    return state.all
  },
  transaction (state) {
    return state.current
  },
  educatorTransaction (state) {
    return state.currentEducator
  }
}

export default {
  state,
  actions,
  mutations,
  getters
}
