import blockexplorer from '@/api/blockexplorer'

const state = {
  all: [],
  current: {},
  perPage: 1,
  total: 0
}

const actions = {
  getAllBlocks ({commit}, page = 1, perPage = state.perPage) {
    blockexplorer.getBlocks(Blocks => {
      commit('setBlocks', Blocks)
    }, page, perPage)
  },
  getBlock ({commit, hash}) {
    blockexplorer.getBlock(Block => {
      commit('setCurrentBlock', Block)
    }, hash)
  },
  getTotal ({commit}) {
    blockexplorer.getBlocks(Blocks => {
      commit('setTotal', Blocks[0])
    }, 1, 1)
  }
}

const mutations = {
  setBlocks (state, blocks) {
    state.all = blocks
  },
  setCurrentBlock (state, block) {
    state.current = block
  },
  setTotal (state, block) {
    state.total = Math.ceil(block.header.difficulty / state.perPage)
  }
}

const getters = {
  blocks (state) {
    return state.all
  },
  block (state) {
    return state.current
  },
  totalPages (state) {
    return state.total
  },
  perPage (state) {
    return state.perPage
  }
}

export default {
  state,
  actions,
  mutations,
  getters
}
