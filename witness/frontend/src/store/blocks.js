import blockexplorer from '@/api/blockexplorer'

const state = {
  all: [],
  current: {}
}

const actions = {
  getAllBlocks ({commit}) {
    blockexplorer.getBlocks(Blocks => {
      commit('setBlocks', Blocks)
    })
  },
  getBlock ({commit, hash}) {
    blockexplorer.getBlock(Block => {
      commit('setCurrentBlock', Block)
    }, hash)
  }
}

const mutations = {
  setBlocks (state, blocks) {
    state.all = blocks
  },
  setCurrentBlock (state, block) {
    state.current = block
  }
}

const getters = {
  blocks (state) {
    return state.all
  },
  block (state) {
    return state.current
  }
}

export default {
  state,
  actions,
  mutations,
  getters
}
