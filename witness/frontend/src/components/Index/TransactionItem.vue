<template>
  <div class="pseudoTable__row" :class="{ ' pseudoTable__row--student': isPublication(transaction) }">
      <div class="pseudoTable__cell cell cell__hash hash">
        <router-link
          :to="{ name: isMoney(transaction) ? 'moneyTransactionShow' : 'publicationTransactionShow', params: {hash: transaction.txId} }"
          class="link">
          {{ transaction.txId }}
        </router-link>
      </div>
      <div class="pseudoTable__cell cell cell__time">{{ new Date(transaction.block.since/1000) | moment('DD/MM/YYYY HH:MM:SS') }}</div>
      <div class="pseudoTable__cell cell cell__totalSent" v-if="isMoney(transaction)">
        <dscp-format :value="transaction.outValue"/>
      </div>
  </div>
</template>

<script>
import DscpFormat from '@/components/DscpFormat'

export default {
  name: 'TransactionItem',
  props: {
    transaction: Object
  },
  components: { DscpFormat },
  methods: {
    isMoney: (transaction) => transaction.txType === 'money',
    isPublication: (transaction) => transaction.txType === 'publication',
    routeName: (transaction) => this.isMoney(transaction) ? 'moneyTransactionShow' : 'publicationTransactionShow'
  }
}
</script>
