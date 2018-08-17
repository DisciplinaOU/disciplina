<template>
  <div>
    <div class="pseudoTableWrapper">
        <div class="pseudoTable">
            <transaction-item v-for="transaction in transactions" :key="transaction.txId" :transaction="transaction"/>
        </div>
    </div>
    <div class="paginationBlock marginTop40">
        <div class="paginationBlock__btnBack btn btn--more" v-if="nextTransactionHash" @click="getAllTransactions(nextTransactionHash)">Show more</div>
    </div>
  </div>
</template>

<script>
import { mapGetters, mapActions } from 'vuex'
import TransactionItem from './TransactionItem'

export default {
  name: 'TransactionList',
  components: { TransactionItem },
  data () {
    return {
      currentPage: 1
    }
  },
  mounted () {
    if (this.currentPage === 1) {
      return this.getAllTransactions()
    }
  },
  computed: {
    ...mapGetters([
      'transactions',
      'nextTransactionHash'
    ])
  },
  methods: {
    ...mapActions([
      'getAllTransactions'
    ])
  }
}
</script>
