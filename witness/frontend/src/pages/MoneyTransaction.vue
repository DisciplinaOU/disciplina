<template>
  <div class="main mainTransaction" v-if="moneyLoaded">
      <div class="transaction marginTop30">
          <div class="container">
              <h3 class="lastSlots__title blockTitle marginBottom30">Transaction</h3>
              <div class="transactionInformBlock">
                  <div class="transactionInformBlock__title">
                      <p class="transactionInformBlock__hash hash">{{ moneyTransaction.txId }}</p>
                      <p class="transactionInformBlock__totalSent btn btn--blue">
                        <dscp-format :value="moneyTransaction.outValue"/>
                      </p>
                  </div>
                  <div class="transactionInformBlock__fromTo">
                      <div class="transactionInformBlock__from informBlock">
                          <p class="informBlock__title">From</p>
                          <p class="informBlock__hash hash">{{ moneyTransaction.money.inAcc.addr }}</p>
                      </div>
                      <div class="transactionInformBlock__to informBlock">
                          <p class="informBlock__title">To</p>
                          <p class="informBlock__hash hash">DdzFFzCqrhsezA9Ue95vSMUmU9KvJ2AHkvoRKMCfoajLnqyPdeeGjMiQYGgf1zjoc33BhEGkiQZG78JWjSwldsfkjslfkjwel</p>
                          <p class="informBlock__hash hash">DdzFFzCqrhsezA9Ue95vSMUmU9KvJ2AHkvoRKMCfoajLnqyPdeeGjMiQYGgf1zjoc33BhEGkiQZG78JWjSwldsfkjslfkjwel</p>
                          <p class="informBlock__hash hash">DdzFFzCqrhsezA9Ue95vSMUmU9KvJ2AHkvoRKMCfoajLnqyPdeeGjMiQYGgf1zjoc33BhEGkiQZG78JWjSwldsfkjslfkjwel</p>
                      </div>
                  </div>
              </div>
              <div class="transaction__summary marginTop30 summary">
                  <div class="summary__title">Summary</div>
                  <div class="summary__items itemsSummary">
                      <div class="itemsSummary_item itemSummary itemSummary--date">
                          <p class="itemSummary__title">Received time</p><p class="itemSummary__content">07/16/2018 10:53:11</p>
                      </div>
                      <div class="itemsSummary_item itemSummary itemSummary--included">
                          <p class="itemSummary__title">Included in</p><p class="itemSummary__content">Epoch 59 / Slot 2365</p>
                      </div>
                      <div class="itemsSummary_item itemSummary itemSummary--output">
                          <p class="itemSummary__title">Total Output</p><p class="itemSummary__content">139,638<span class="small">.439587</span> DSCP</p>
                      </div>
                      <div class="itemsSummary_item itemSummary itemSummary--fee">
                          <p class="itemSummary__title">Transaction fee</p><p class="itemSummary__content">19,638<span class="small">.439587</span> DSCP</p>
                      </div>
                  </div>
              </div>
          </div>
      </div>
  </div>
</template>

<script>
import DscpFormat from '@/components/DscpFormat'
import { mapGetters, mapActions } from 'vuex'

export default {
  name: 'Transaction',
  components: { DscpFormat },
  beforeMount () {
    this.getMoneyTransaction(this.$route.params.hash)
  },
  computed: {
    ...mapGetters([
      'moneyTransaction',
      'moneyLoaded'
    ])
  },
  methods: {
    ...mapActions([
      'getMoneyTransaction'
    ])
  }
}
</script>
