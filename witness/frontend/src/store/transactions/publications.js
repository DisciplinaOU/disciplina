import blockexplorer from '@/api/blockexplorer'

const state = {
  loaded: false,
  current: {}
}

const actions = {
  getPublicationTransaction ({commit}, hash) {
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
  publicationTransaction (state) {
    return state.current
  },
  publicationLoaded (state) {
    return state.loaded
  }
}

export default {
  state,
  actions,
  mutations,
  getters
}
