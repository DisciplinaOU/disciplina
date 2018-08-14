import blockexplorer from '@/api/blockexplorer'

const state = {
  all: []
}

const actions = {
  getAllCourses ({commit}) {
    blockexplorer.getBlocks(Blocks => {
      commit('recieveBlocks', Blocks)
    })
  }
}

const mutations = {
  recieveBlocks (state, blocks) {
    state.all = blocks
  }
}

const getters = {
  allBlocks (state) {
    return state.all
  }
}

export default {
  state,
  actions,
  mutations,
  getters
}
