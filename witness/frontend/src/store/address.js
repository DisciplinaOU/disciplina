import blockexplorer from '@/api/blockexplorer'

const state = {
  current: {},
  loaded: false
}

const actions = {
  getAddress ({commit}, hash) {
    blockexplorer.getAddress(Address => {
      commit('setAddress', Address)
    }, hash)
  }
}

const mutations = {
  setAddress (state, address) {
    state.current = address
    state.loaded = true
  }
}

const getters = {
  address (state) {
    return state.current
  },
  loaded (state) {
    return state.loaded
  }
}

export default {
  state,
  actions,
  mutations,
  getters
}
