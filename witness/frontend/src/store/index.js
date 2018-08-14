import Vuex from 'vuex'
import Vue from 'vue'
import blocks from './blocks'
// import assignments from './assignments'
// import user from './user'

Vue.use(Vuex)

export default new Vuex.Store({
  modules: {
    blocks
  }
})
