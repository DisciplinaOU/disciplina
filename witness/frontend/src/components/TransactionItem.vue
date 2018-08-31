<template>
  <money :transaction="transaction" :timestamp="timestamp" :currentAddress="currentAddress" v-if="isMoney(transaction)"/>
  <publication :transaction="transaction" :timestamp="timestamp" :currentAddress="currentAddress" v-else-if="isPublication(transaction)"/>
</template>

<script>
import Money from '@/components/TransactionItem/Money'
import Publication from '@/components/TransactionItem/Publication'

export default {
  name: 'TransactionItem',
  props: {
    transaction: Object,
    timestamp: Number,
    currentAddress: String
  },
  components: { Money, Publication },
  methods: {
    isMoney: (transaction) =>
      transaction.txType === 'money',
    isPublication: (transaction) =>
      transaction.txType === 'publication'
  }
}
</script>
