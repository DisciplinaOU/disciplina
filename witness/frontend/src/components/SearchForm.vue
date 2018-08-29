<template>
  <form action="#" v-on:submit.prevent="searchHash">
      <div class="mainForm">
          <input type="text" class="mainForm__input" v-model="query" :placeholder="$t('placeholder')">
          <button class="mainForm__send">{{ $t('buttonText') }}</button>
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
  i18nOptions: {
    keyPrefix: 'search'
  },
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
        } else {
          this.$router.push({name: '404', query: { query: this.query }})
        }
      }, this.query)
    }
  }
}
</script>
