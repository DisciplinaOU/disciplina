import blockexplorer from '@/api/blockexplorer'

const state = {
  all: [],
  current: {},
  perPage: 10,
  currentPage: 1,
  totalPages: 0,
  firstBlockDifficulty: 0,
  fromBlockHash: '',
  fromBlockHashPrev: '',
  loaded: false
}

const actions = {
  getAllBlocks ({commit}, from, count = state.perPage + 1) {
    blockexplorer.getBlocks((Blocks) => {
      const lastBlock = Blocks.pop()
      commit('setFromBlockHashPrev', Blocks[0])
      commit('setBlocks', Blocks)
      commit('calcTotal', Blocks[0])
      commit('setFromBlockHash', lastBlock)
    }, from, count)
  },
  getBlock ({commit}, hash) {
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
    state.loaded = true
  },
  calcTotal (state, block) {
    if (state.totalPages === 0) {
      state.firstBlockDifficulty = block.header.difficulty
      state.totalPages = Math.ceil(state.firstBlockDifficulty / state.perPage)
    }
  },
  setFromBlockHash (state, block) {
    state.fromBlockHash = block.headerHash
  },
  setFromBlockHashPrev (state, block) {
    if (state.all.length && block.header.difficulty !== state.firstBlockDifficulty) {
      state.fromBlockHashPrev = state.all[0].headerHash
    }
  },
  setPage (state, page) {
    state.currentPage = page
  }
}

const getters = {
  blocks (state) {
    return state.all
  },
  block (state) {
    return state.current
  },
  blockLoaded (state) {
    return state.loaded
  },
  totalPages (state) {
    return state.totalPages
  },
  perPage (state) {
    return state.perPage
  },
  fromBlockHash (state) {
    return state.fromBlockHash
  },
  fromBlockHashPrev (state) {
    return state.fromBlockHashPrev
  },
  currentPage (state) {
    return state.currentPage
  }
}

export default {
  state,
  actions,
  mutations,
  getters
}
