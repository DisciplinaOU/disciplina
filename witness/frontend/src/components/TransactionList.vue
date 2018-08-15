<template>
  <div class="transactions marginTop30">
    <div class="container">
      <h3 class="lastSlots__title blockTitle marginBottom40">Transaction</h3>
      <transaction-item
        v-for="transaction in transactions"
        :transaction="transaction"
        :key="transaction.txId"/>
      <div class="paginationBlock marginTop40" v-if="false">
        <div class="paginationBlock__btnBack btn btn--more">Show more</div>
      </div>
    </div>
  </div>
</template>

<script>
import TransactionItem from '@/components/TransactionItem'

export default {
  name: 'TransactionList',
  props: ['transactions'],
  components: { TransactionItem }
}
</script>
