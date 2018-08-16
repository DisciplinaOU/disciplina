<template>
  <div class="main eduTransaction" v-if="loaded">
    <div class="transaction marginTop30">
      <div class="container">
        <h3 class="lastSlots__title blockTitle marginBottom30">Educator transaction</h3>
        <div class="transactionInformBlock">
          <div class="transactionInformBlock__title">
            <p class="transactionInformBlock__hash hash">{{ publicationTransaction.txId }}</p>
            <p class="transactionInformBlock__date">07/16/2018 09:33:11</p>
          </div>
          <div class="transactionInformBlock__fromTo">
            <div class="transactionInformBlock__from informBlock">
              <p class="informBlock__title">Educator</p>
              <p class="informBlock__hash hash">{{ publicationTransaction.publication.author }}</p>
            </div>
            <div class="transactionInformBlock__to informBlock">
              <p class="informBlock__title">Update</p>
              <p class="informBlock__hash hash">{{ publicationTransaction.publication.block }}</p>
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
            <div class="itemsSummary_item itemSummary itemSummary--fee">
              <p class="itemSummary__title">Transaction fee</p><p class="itemSummary__content">28 DSCP</p>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import { mapGetters, mapActions } from 'vuex'

export default {
  name: 'PublicationTransaction',
  beforeRouteEnter (to, from, next) {
    next(vm => {
      vm.getPublicationTransaction(to.params.hash)
    })
  },
  computed: {
    ...mapGetters([
      'publicationTransaction',
      'loaded'
    ])
  },
  methods: {
    ...mapActions([
      'getPublicationTransaction'
    ])
  }
}
</script>
