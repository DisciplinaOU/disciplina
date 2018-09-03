// The Vue build version to load with the `import` command
// (runtime-only or standalone) has been set in webpack.base.conf with an alias.
import Vue from 'vue'
import App from './App'

import axios from 'axios'
import VueAxios from 'vue-axios'

import i18next from 'i18next'
import VueI18Next from '@panter/vue-i18next'
import En from './locales/en-US.json'
import Zh from './locales/zh-CN.json'
import Ja from './locales/ja-JP.json'
import Ko from './locales/ko-KR.json'

import router from './router'
import store from './store/index'
import './assets/css/index.scss'

Vue.use(VueAxios, axios)
const apiBaseUrl = process.env.WITNESS_API_URL || 'https://witness.disciplina.io'
Vue.axios.defaults.baseURL = apiBaseUrl + '/api/witness/v1'
Vue.axios.defaults.headers.common['Content-Type'] = 'application/json'

Vue.use(VueI18Next)
i18next.init({
  lng: 'en',
  fallbackLng: 'en',
  resources: {
    en: { translation: En },
    zh: { translation: Zh },
    ja: { translation: Ja },
    ko: { translation: Ko }
  }
})
const i18n = new VueI18Next(i18next)

Vue.use(require('vue-moment'))

Vue.config.productionTip = false

/* eslint-disable no-new */
new Vue({
  el: '#app',
  router,
  store,
  components: { App },
  template: '<App/>',
  i18n: i18n
})
