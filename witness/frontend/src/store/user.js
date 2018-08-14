import student from '@/api/student'

const state = {
  user: {
    name: '',
    lastName: '',
    avatarUrl: '',
    cbUrl: ''
  }
}

const actions = {
  getUser ({commit}) {
    student.getUser(User => {
      commit('recieveUser', User)
    })
  }
}

const mutations = {
  recieveUser (state, user) {
    state.user = user
  }
}

const getters = {
  user (state) {
    return state.user
  }
}

export default {
  state,
  actions,
  mutations,
  getters
}
