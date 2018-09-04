import Vue from 'vue'
import Router from 'vue-router'

import Base from '@/pages/Base'
import Index from '@/pages/Index'
import Block from '@/pages/Block'
import Address from '@/pages/Address'
import MoneyTransaction from '@/pages/MoneyTransaction'
import NotFound from '@/pages/NotFound'
// import PublicationTransaction from '@/pages/PublicationTransaction'

Vue.use(Router)

export default new Router({
  routes: [
    {
      path: '/',
      component: Base,
      children: [
        {
          path: '',
          name: 'index',
          component: Index
        },
        {
          path: '/block/:hash',
          name: 'blockShow',
          component: Block
        },
        {
          path: '/address/:hash',
          name: 'addressShow',
          component: Address
        },
        {
          path: '/money-transaction/:hash',
          name: 'moneyTransactionShow',
          component: MoneyTransaction
        },
        {
          path: '/404',
          name: '404',
          component: NotFound
        }
        // {
        //   path: '/publication-transaction/:hash',
        //   name: 'publicationTransactionShow',
        //   component: PublicationTransaction
        // }
      ]
    }
  ]
})
