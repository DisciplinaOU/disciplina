<template>
  <div class="pseudoTable__row cursorPointer" :class="{ ' pseudoTable__row--student': isPublication(transaction) }" @click="goToTransaction">
      <div class="pseudoTable__cell cell cell__hash hash">
          {{ transaction.txId }}
      </div>
      <div class="pseudoTable__cell cell cell__time">
        <datetime :dt="transaction.block.since/1000"/>
      </div>
      <div class="pseudoTable__cell cell cell__totalSent" v-if="isMoney(transaction)">
        <dscp-format :value="transaction.outValue"/>
      </div>
  </div>
</template>

<script>
import DscpFormat from '@/components/DscpFormat'
import Datetime from '@/components/Datetime'

export default {
  name: 'TransactionItem',
  props: {
    transaction: Object
  },
  components: { DscpFormat, Datetime },
  methods: {
    isMoney: (transaction) => transaction.txType === 'money',
    isPublication: (transaction) => transaction.txType === 'publication',
    routeName (transaction) {
      return this.isMoney(transaction) ? 'moneyTransactionShow' : 'publicationTransactionShow'
    },
    goToTransaction () {
      this.$router.push({name: this.routeName(this.transaction), params: {hash: this.transaction.txId}})
    }
  }
}
</script>
