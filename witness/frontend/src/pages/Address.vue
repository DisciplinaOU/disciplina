<template>
  <div class="main address" v-if="loaded">
    <div class="addressBlock">
      <div class="container">
        <h3 class="addressBlock__title blockTitle marginBottom30 marginTop30">Address</h3>
        <div class="addressBlock__item addressItem">
          <div class="addressItem__inform">
            <p class="addressItem__title">Address</p>
            <p class="addressItem__value hash">{{ $route.params.hash }}</p>
          </div>
          <div class="addressItem__inform">
            <p class="addressItem__title">Transactions</p>
            <p class="addressItem__value">{{ address.transactionCount }}</p>
          </div>
          <div class="addressItem__inform">
            <p class="addressItem__title">Final balace</p>
            <p class="addressItem__value">{{ address.balances.confirmed }} DSCP</p>
          </div>
        </div>
      </div>
    </div>
    <transaction-list :transactions="address.transactions" v-if="address.transactions"/>
  </div>
</template>

<script>
import { mapActions, mapGetters } from 'vuex'
import TransactionList from '@/components/TransactionList'

export default {
  name: 'Address',
  computed: {
    ...mapGetters([
      'address',
      'loaded'
    ])
  },
  mounted () {
    this.getAddress(this.$route.params.hash)
  },
  beforeRouteUpdate (to, from, next) {
    this.getAddress(to.params.hash)
    next()
  },
  components: { TransactionList },
  methods: {
    ...mapActions([
      'getAddress'
    ])
  }
}
</script>
