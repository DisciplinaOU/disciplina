import student from '@/api/student'

const state = {
  all: []
}

const actions = {
  getAllAssignments ({commit}) {
    student.getAssignments(assignments => {
      commit('recieve_assignments', assignments)
    })
  }
}

const mutations = {
  recieve_assignments (state, assignments) {
    state.all = assignments
  }
}

const getters = {
  allAssignments (state) {
    return state.all
  }
}

export default {
  state,
  actions,
  mutations,
  getters
}
