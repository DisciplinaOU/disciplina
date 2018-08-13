const _assignments = [
  { id: 1, desc: 'Assignment name 1', isFinal: false, lastSubmission: { assignmentHash: 'foo bar', grade: { grade: 10, hasProof: true } } },
  { id: 2, desc: 'Assignment name 2', isFinal: false, lastSubmission: { assignmentHash: 'foo bar', grade: { grade: 50, hasProof: true } } },
  { id: 3, desc: 'Assignment name 3', isFinal: false, lastSubmission: { assignmentHash: 'foo bar', grade: { grade: 100 } } },
  { id: 4, desc: 'Assignment name 4', isFinal: false, lastSubmission: { assignmentHash: 'foo bar' } },
  { id: 5, desc: 'Assignment name 5', isFinal: true }
]

const _courses = [
  { id: 1, desc: 'Course 1', subjects: [], isEnrolled: true },
  { id: 2, desc: 'Course 2', subjects: [], isEnrolled: true },
  { id: 3, desc: 'Course 3', subjects: [], isEnrolled: true },
  { id: 4, desc: 'Course 4', subjects: [], isEnrolled: true }
]

const _user = {
  name: 'John',
  lastName: 'Doe',
  avatarUrl: '../assets/img/ava.png',
  cvUrl: 'https://disciplina.io',
  disciplinaAddress: 'disciplina.io/student/343334dde3223dcv'
}

export default {
  getCourses (cb) {
    setTimeout(() => cb(_courses), 100)
  },

  getAssignments (cb) {
    setTimeout(() => cb(_assignments), 100)
  },

  getUser (cb) {
    setTimeout(() => cb(_user), 100)
  }
}
