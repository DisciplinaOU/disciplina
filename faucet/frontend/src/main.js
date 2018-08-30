// The Vue build version to load with the `import` command
// (runtime-only or standalone) has been set in webpack.base.conf with an alias.
import Vue from 'vue'
import App from './App'

import axios from 'axios'

import i18next from 'i18next'
import VueI18Next from '@panter/vue-i18next'
import En from './locales/en'
import Zh from './locales/zh'
import Jp from './locales/jp'
import Kr from './locales/kr'

import './assets/css/index.scss'

const apiBaseUrl = process.env.FAUCET_API_URL || 'https://faucet.disciplina.io'
Vue.prototype.$axios = axios.create({
  baseURL: apiBaseUrl + '/api/witness/v1'
})
Vue.prototype.$axios.defaults.headers.post['Content-Type'] = 'application/json'

Vue.use(VueI18Next)
i18next.init({
  lng: 'en',
  resources: {
    en: { translation: En },
    zh: { translation: Zh },
    jp: { translation: Jp },
    kr: { translation: Kr }
  }
})
const i18n = new VueI18Next(i18next)

Vue.config.productionTip = false

/* eslint-disable no-new */
new Vue({
  el: '#app',
  components: { App },
  template: '<App/>',
  i18n: i18n
})
