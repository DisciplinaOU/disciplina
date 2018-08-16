// The Vue build version to load with the `import` command
// (runtime-only or standalone) has been set in webpack.base.conf with an alias.
import Vue from 'vue'
import App from './App'

import axios from 'axios'
import VueAxios from 'vue-axios'

import router from './router'
import store from './store/index'
import './assets/css/index.scss'

Vue.use(VueAxios, axios)
Vue.axios.defaults.baseURL = 'https://witness-1.disciplina.serokell.team/api/witness/v1'
Vue.axios.defaults.headers.common['Content-Type'] = 'application/json'

Vue.use(require('vue-moment'))

Vue.config.productionTip = false

/* eslint-disable no-new */
new Vue({
  el: '#app',
  router,
  store,
  components: { App },
  template: '<App/>'
})
