import blockexplorer from '@/api/blockexplorer'

const state = {
  all: [],
  current: {},
  perPage: 10,
  currentPage: 1,
  totalPages: 0,
  firstBlockIndex: 0,
  fromBlockHash: '',
  fromBlockHashPrev: '',
  loaded: false
}

const actions = {
  getAllBlocks ({commit}, page = state.currentPage) {
    blockexplorer.getBlocks((totalBlocks, Blocks) => {
      if (state.all.length === 0) {
        setAutoupdateList(this)
      }
      commit('setBlocks', Blocks)
      commit('setTotal', totalBlocks)
    }, (page - 1) * state.perPage, state.perPage)
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
  setTotal (state, total) {
    state.totalPages = Math.ceil(total / state.perPage)
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
  currentPage (state) {
    return state.currentPage
  }
}

function setAutoupdateList (store) {
  setInterval(() => {
    if (store.getters.currentPage === 1) {
      store.dispatch('getAllBlocks')
    }
  }, 20000)
}

export default {
  state,
  actions,
  mutations,
  getters
}
