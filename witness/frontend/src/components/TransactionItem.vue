<template>
  <money :transaction="transaction" v-if="isMoney(transaction)"/>
  <educator :transaction="transaction" v-else-if="isEducator(transaction)"/>
</template>

<script>
import Money from '@/components/TransactionItem/Money'
import Educator from '@/components/TransactionItem/Educator'

export default {
  name: 'TransactionItem',
  props: {
    transaction: Object
  },
  components: { Money, Educator },
  methods: {
    isMoney: (transaction) =>
      transaction.txType === 'money',
    isEducator: (transaction) =>
      transaction.txType === 'educator'
  }
}
</script>
