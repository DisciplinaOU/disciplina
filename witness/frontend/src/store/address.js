import blockexplorer from '@/api/blockexplorer'

const state = {
  current: {
    transactions: [],
    balances: {}
  }
}

const actions = {
  getAddress ({commit, hash}) {
    blockexplorer.getAddress(Address => {
      commit('setAddress', Address)
    })
  }
}

const mutations = {
  setAddress (state, address) {
    state.current = address
  }
}

const getters = {
  address (state) {
    return state.current
  }
}

export default {
  state,
  actions,
  mutations,
  getters
}
