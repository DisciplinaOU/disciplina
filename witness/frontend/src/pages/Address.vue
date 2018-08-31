<template>
  <div class="main address" v-if="loaded">
    <div class="addressBlock">
      <div class="container">
        <h3 class="addressBlock__title blockTitle marginBottom30 marginTop30">{{ $t("title") }}</h3>
        <div class="addressBlock__item addressItem">
          <div class="addressItem__inform">
            <p class="addressItem__title">{{ $t("title") }}</p>
            <p class="addressItem__value hash">{{ $route.params.hash }}</p>
          </div>
          <div class="addressItem__inform">
            <p class="addressItem__title">{{ $t("transactions") }}</p>
            <p class="addressItem__value">{{ address.transactionCount }}</p>
          </div>
          <div class="addressItem__inform">
            <p class="addressItem__title">{{ $t("finalBalance") }}</p>
            <p class="addressItem__value">{{ address.balances.confirmed }} DSCP</p>
          </div>
        </div>
      </div>
    </div>
    <transaction-list :transactions="reservedTransactions" :currentAddress="currentAddress" v-if="address.transactions"/>
  </div>
</template>

<script>
import { mapActions, mapGetters } from 'vuex'
import TransactionList from '@/components/TransactionList'

export default {
  name: 'Address',
  i18nOptions: {
    keyPrefix: 'address'
  },
  computed: {
    ...mapGetters([
      'address',
      'loaded'
    ]),
    reservedTransactions () {
      return this.address.transactions.slice().reverse()
    },
    currentAddress () {
      return this.$route.params.hash
    }
  },
  mounted () {
    this.getAddress(this.currentAddress)
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
