<template>
  <form action="#" v-on:submit.prevent="searchHash">
      <div class="mainForm">
          <input type="text" class="mainForm__input" v-model="query" placeholder="Search addresses, transactions, epochs & slots on the DISCIPLINA network">
          <button class="mainForm__send">Search</button>
      </div>
  </form>
</template>

<script>
import api from '@/api/blockexplorer'
const ROUTE_BY_TYPE = {
  block: 'blockShow',
  address: 'addressShow',
  'money-transaction': 'moneyTransactionShow',
  'publication-transaction': false,
  unknown: false
}

export default {
  name: 'SearchForm',
  data () {
    return {
      query: ''
    }
  },
  methods: {
    searchHash () {
      api.getType((type) => {
        if (ROUTE_BY_TYPE[type]) {
          this.$router.push({name: ROUTE_BY_TYPE[type], params: { hash: this.query }})
        }
      }, this.query)
    }
  }
}
</script>
