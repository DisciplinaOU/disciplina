<template>
  <div>
    <div class="pseudoTableWrapper">
        <div class="pseudoTable">
            <transaction-item v-for="transaction in transactions" :key="transaction.txId" :transaction="transaction"/>
        </div>
    </div>
    <div class="paginationBlock marginTop40">
        <div class="paginationBlock__btnBack btn btn--more" v-if="!listFinished" @click="fetchTransactions(currentPage + 1)">Show more</div>
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
      'listFinished'
    ])
  },
  methods: {
    ...mapActions([
      'getAllTransactions',
      'checkListFinished'
    ]),
    fetchTransactions (page) {
      if (page < 1) {
        return false
      }

      this.getAllTransactions(page)
      this.checkListFinished(page)
      this.currentPage = page
    }
  }
}
</script>
