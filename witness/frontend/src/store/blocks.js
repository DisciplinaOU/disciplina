import blockexplorer from '@/api/blockexplorer'

const state = {
  all: []
}

const actions = {
  getAllBlocks ({commit}) {
    blockexplorer.getBlocks(Blocks => {
      commit('setBlocks', Blocks)
    })
  }
}

const mutations = {
  setBlocks (state, blocks) {
    state.all = blocks
  }
}

const getters = {
  blocks (state) {
    return state.all
  }
}

export default {
  state,
  actions,
  mutations,
  getters
}
