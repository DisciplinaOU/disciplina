import Vue from 'vue'
import Router from 'vue-router'

import Base from '@/pages/Base'
import Index from '@/pages/Index'
import Block from '@/pages/Block'
import Transaction from '@/pages/Transaction'
import Address from '@/pages/Address'
import EduTransaction from '@/pages/EduTransaction'

Vue.use(Router)

export default new Router({
  routes: [

    {
      path: '/',
      component: Base,
      children: [
        {
          path: '',
          component: Index
        },
        {
          path: '/block/:hash',
          component: Block
        },
        {
          path: '/transaction/:hash',
          component: Transaction
        },
        {
          path: '/address/:hash',
          component: Address
        },
        {
          path: '/edutransaction/:hash',
          component: EduTransaction
        }
      ]
    }
  ]
})
