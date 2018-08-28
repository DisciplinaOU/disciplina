import blockexplorer from '@/api/blockexplorer'

const state = {
  loaded: false,
  current: {}
}

const actions = {
  getMoneyTransaction ({commit}, hash) {
    blockexplorer.getTransaction(Transaction => {
      commit('setCurrent', Transaction)
    }, hash)
  }
}

const mutations = {
  setCurrent (state, transaction) {
    state.current = transaction
    state.loaded = true
  }
}

const getters = {
  moneyTransaction (state) {
    return state.current
  },
  moneyLoaded (state) {
    return state.loaded
  }
}

export default {
  state,
  actions,
  mutations,
  getters
}
